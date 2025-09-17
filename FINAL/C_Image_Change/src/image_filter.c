// image_filter.c
// C version of the ARM64 assembly image filter with multithreading and buffers
// Modified to include bicubic downsampling, grayscale conversion, and 8bpp output
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <math.h>
#include <stdio.h>

// Define STB_IMAGE_IMPLEMENTATION to include the function definitions
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

// Max 720p processing resolution
#define MAX_WIDTH 1280
#define MAX_HEIGHT 720

// Buffers
static uint8_t  input_image_buffer[MAX_WIDTH * MAX_HEIGHT];       // 8 bpp grayscale input
static uint8_t  x_filtered_buffer[MAX_WIDTH * MAX_HEIGHT];       // 8-bit per pixel (Sobel X)
static uint8_t  y_filtered_buffer[MAX_WIDTH * MAX_HEIGHT];       // 8-bit per pixel (Sobel Y)
static double   combined_output_buffer[MAX_WIDTH * MAX_HEIGHT];  // double precision output

// Image metadata
static int input_width = 0;
static int input_height = 0;
static int input_bpp = 0;

// Filter done flags
static volatile int x_filter_done = 0;
static volatile int y_filter_done = 0;

// Function prototypes
void set_input_image(const char* filepath);
double* get_image_matrix(int* rows, int* cols);
void* filter_x_thread(void* arg);
void* filter_y_thread(void* arg);
void combine_results(void);
void downsample_bicubic(const uint8_t* src, int src_w, int src_h, uint8_t* dest, int dest_w, int dest_h, int bytes_per_pixel);
void convert_to_grayscale(const uint8_t* src, uint8_t* dest, int width, int height, int bpp);

// -------------------------------------------
// Sets the input image from a file path
// -------------------------------------------
void set_input_image(const char* filepath) {
    int original_width, original_height, original_channels;
    // Load the image into a temporary buffer. STBI_rgb_alpha handles up to 32bpp.
    uint8_t* original_data = stbi_load(filepath, &original_width, &original_height, &original_channels, STBI_rgb_alpha);

    if (original_data == NULL) {
        fprintf(stderr, "Error loading image: %s\n", stbi_failure_reason());
        exit(1);
    }
    
    int new_width = original_width;
    int new_height = original_height;
    int new_bytes_per_pixel = original_channels;
    uint8_t* temp_downsampled = NULL;

    // Check if downsampling is needed
    if (original_width > MAX_WIDTH || original_height > MAX_HEIGHT) {
        float ratio = fmin((float)MAX_WIDTH / original_width, (float)MAX_HEIGHT / original_height);
        new_width = (int)(original_width * ratio);
        new_height = (int)(original_height * ratio);

        printf("Downsampling image from %dx%d to %dx%d\n", original_width, original_height, new_width, new_height);

        temp_downsampled = (uint8_t*)malloc(new_width * new_height * new_bytes_per_pixel);
        if (!temp_downsampled) {
            fprintf(stderr, "Failed to allocate temporary buffer for downsampling!\n");
            stbi_image_free(original_data);
            exit(1);
        }
        downsample_bicubic(original_data, original_width, original_height, temp_downsampled, new_width, new_height, new_bytes_per_pixel);
    }

    // Allocate buffer for grayscale conversion
    uint8_t* temp_grayscale = (uint8_t*)malloc(new_width * new_height);
    if (!temp_grayscale) {
        fprintf(stderr, "Failed to allocate temporary buffer for grayscale conversion!\n");
        stbi_image_free(original_data);
        if(temp_downsampled) free(temp_downsampled);
        exit(1);
    }
    
    // Perform grayscale conversion
    if (temp_downsampled) {
        convert_to_grayscale(temp_downsampled, temp_grayscale, new_width, new_height, new_bytes_per_pixel * 8);
        free(temp_downsampled);
    } else {
        convert_to_grayscale(original_data, temp_grayscale, new_width, new_height, original_channels * 8);
    }

    // Copy converted data to the main buffer
    memcpy(input_image_buffer, temp_grayscale, (size_t)new_width * new_height);
    free(temp_grayscale);

    // Free the original image data loaded by stb_image
    stbi_image_free(original_data);

    // Update metadata to the new dimensions and BPP
    input_width = new_width;
    input_height = new_height;
    input_bpp = 8; // Now fixed at 8 bpp

    // Reset done flags
    x_filter_done = 0;
    y_filter_done = 0;
}

// -------------------------------------------
// Converts a multi-channel image to 8-bit grayscale
// -------------------------------------------
void convert_to_grayscale(const uint8_t* src, uint8_t* dest, int width, int height, int bpp) {
    int bytes_per_pixel = bpp / 8;
    for (int i = 0; i < width * height; i++) {
        uint8_t r = src[i * bytes_per_pixel];
        uint8_t g = src[i * bytes_per_pixel + 1];
        uint8_t b = src[i * bytes_per_pixel + 2];
        
        dest[i] = (uint8_t)(0.299 * r + 0.587 * g + 0.114 * b);
    }
}

// -------------------------------------------
// Simple cubic interpolation helper function
// -------------------------------------------
float cubic_interpolation(float v0, float v1, float v2, float v3, float t) {
    float a0 = v3 - v2 - v0 + v1;
    float a1 = v0 - v1 - a0;
    float a2 = v2 - v0;
    float a3 = v1;
    return a0 * t * t * t + a1 * t * t + a2 * t + a3;
}

// -------------------------------------------
// Bicubic downsampling function
// -------------------------------------------
void downsample_bicubic(const uint8_t* src, int src_w, int src_h, uint8_t* dest, int dest_w, int dest_h, int bytes_per_pixel) {
    float x_ratio = (float)(src_w - 1) / dest_w;
    float y_ratio = (float)(src_h - 1) / dest_h;

    for (int y = 0; y < dest_h; y++) {
        for (int x = 0; x < dest_w; x++) {
            float src_x = x * x_ratio;
            float src_y = y * y_ratio;

            int x_int = (int)src_x;
            int y_int = (int)src_y;
            float x_frac = src_x - x_int;
            float y_frac = src_y - y_int;

            for (int c = 0; c < bytes_per_pixel; c++) {
                float rows[4];
                for (int i = -1; i <= 2; i++) {
                    float cols[4];
                    for (int j = -1; j <= 2; j++) {
                        int current_x = fmin(fmax(x_int + j, 0), src_w - 1);
                        int current_y = fmin(fmax(y_int + i, 0), src_h - 1);
                        cols[j + 1] = src[(current_y * src_w + current_x) * bytes_per_pixel + c];
                    }
                    rows[i + 1] = cubic_interpolation(cols[0], cols[1], cols[2], cols[3], x_frac);
                }
                float result = cubic_interpolation(rows[0], rows[1], rows[2], rows[3], y_frac);
                dest[(y * dest_w + x) * bytes_per_pixel + c] = (uint8_t)fmin(fmax(result, 0), 255);
            }
        }
    }
}

// -------------------------------------------
// Starts filtering threads, waits for them to finish,
// then combines results and returns combined image pointer
// -------------------------------------------
double* get_image_matrix(int* rows, int* cols) {
    pthread_t thread_x, thread_y;

    // Launch X filter thread
    if (pthread_create(&thread_x, NULL, filter_x_thread, NULL) != 0) {
        perror("Failed to create filter_x_thread");
        exit(1);
    }

    // Launch Y filter thread
    if (pthread_create(&thread_y, NULL, filter_y_thread, NULL) != 0) {
        perror("Failed to create filter_y_thread");
        exit(1);
    }

    // Wait for threads to complete
    pthread_join(thread_x, NULL);
    pthread_join(thread_y, NULL);

    // Combine filtered results
    combine_results();

    // Return pointer and dimensions
    if (rows) *rows = input_height;
    if (cols) *cols = input_width;

    return combined_output_buffer;
}

// -------------------------------------------
// Sobel X filter thread function
// Applies 3x3 Sobel kernel in X direction to input_image_buffer
// Stores 8-bit results in x_filtered_buffer
// -------------------------------------------
void* filter_x_thread(void* arg) {
    int w = input_width;
    int h = input_height;

    // Sobel kernel for X direction
    //  -1 0 +1
    //  -2 0 +2
    //  -1 0 +1

    for (int y = 1; y < h - 1; y++) {
        for (int x = 1; x < w - 1; x++) {
            int gx = 0;

            int p0 = input_image_buffer[(y - 1) * w + (x - 1)];
            int p1 = input_image_buffer[(y - 1) * w + (x + 0)];
            int p2 = input_image_buffer[(y - 1) * w + (x + 1)];

            int p3 = input_image_buffer[(y + 0) * w + (x - 1)];
            int p4 = input_image_buffer[(y + 0) * w + (x + 0)];
            int p5 = input_image_buffer[(y + 0) * w + (x + 1)];

            int p6 = input_image_buffer[(y + 1) * w + (x - 1)];
            int p7 = input_image_buffer[(y + 1) * w + (x + 0)];
            int p8 = input_image_buffer[(y + 1) * w + (x + 1)];

            gx = (-1 * p0) + (0 * p1) + (1 * p2)
               + (-2 * p3) + (0 * p4) + (2 * p5)
               + (-1 * p6) + (0 * p7) + (1 * p8);

            // Clamp result to 8-bit range [0, 255]
            if (gx < 0) gx = 0;
            if (gx > 255) gx = 255;

            x_filtered_buffer[y * w + x] = (uint8_t)gx;
        }
    }

    x_filter_done = 1;
    return NULL;
}

// -------------------------------------------
// Sobel Y filter thread function
// Applies 3x3 Sobel kernel in Y direction to input_image_buffer
// Stores 8-bit results in y_filtered_buffer
// -------------------------------------------
void* filter_y_thread(void* arg) {
    int w = input_width;
    int h = input_height;

    // Sobel kernel for Y direction
    //  +1 +2 +1
    //   0  0  0
    //  -1 -2 -1

    for (int y = 1; y < h - 1; y++) {
        for (int x = 1; x < w - 1; x++) {
            int gy = 0;

            int p0 = input_image_buffer[(y - 1) * w + (x - 1)];
            int p1 = input_image_buffer[(y - 1) * w + (x + 0)];
            int p2 = input_image_buffer[(y - 1) * w + (x + 1)];

            int p3 = input_image_buffer[(y + 0) * w + (x - 1)];
            int p4 = input_image_buffer[(y + 0) * w + (x + 0)];
            int p5 = input_image_buffer[(y + 0) * w + (x + 1)];

            int p6 = input_image_buffer[(y + 1) * w + (x - 1)];
            int p7 = input_image_buffer[(y + 1) * w + (x + 0)];
            int p8 = input_image_buffer[(y + 1) * w + (x + 1)];

            gy = (1 * p0) + (2 * p1) + (1 * p2)
               + (0 * p3) + (0 * p4) + (0 * p5)
               + (-1 * p6) + (-2 * p7) + (-1 * p8);

            // Clamp result to 8-bit range [0, 255]
            if (gy < 0) gy = 0;
            if (gy > 255) gy = 255;

            y_filtered_buffer[y * w + x] = (uint8_t)gy;
        }
    }

    y_filter_done = 1;
    return NULL;
}

// -------------------------------------------
// Combine the X and Y filter buffers into combined_output_buffer
// Uses sqrt(gx^2 + gy^2) normalization for edge magnitude
// -------------------------------------------
void combine_results(void) {
    int w = input_width;
    int h = input_height;

    for (int i = 0; i < w * h; i++) {
        uint8_t gx = x_filtered_buffer[i];
        uint8_t gy = y_filtered_buffer[i];
        combined_output_buffer[i] = sqrt((double)(gx * gx + gy * gy));
    }
}