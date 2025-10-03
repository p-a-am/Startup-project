--int to float
int_to_float:: Int -> Float

int_to_float 0 = 0
--Float to int
float_to_int:: Float -> Int

float_to_int 0 = 0
--random weights at the beginning 
weights:: Int -> [Float]

weights x
    | x > 0 = [(int_to_float x) * 0.8263] ++ weights (x-1)
    | x == 0 = []
--how many parameters
num_of_parameters:: Int -> [Int]

num_of_parameters x =  [x..1]
--basic neuron starting layer
sum_neuron::[Float] -> [Int] -> Float

sum_neuron (b:bs) (x:xs) = b * int_to_float x + sum_neuron bs xs
sum_neuron [] [] = 1
--non-linear Activation function
activation_function:: Float -> Float

activation_function x = 1.0/(1.0 ** (-x))
--complete neuron with z parameters and y starting weights
complete_neuron:: Float -> Int-> Float

complete_neuron y z = activation_function (sum_neuron (weights (float_to_int y)) (num_of_parameters z))
--complete neuron layer with n neurons y to ys weights and z parameters
neuron_layer:: Int -> [Float] -> Int -> [Float]

neuron_layer n (y:ys) z
    |n > 0 = [complete_neuron y z] ++ neuron_layer (n-1) ys z
    |n == 0 = []

--interaction between neuron layers
neuron_layer_interaction::[Float] -> [Float] -> [Float]-> [Float]

neuron_layer_interaction (y:ys) (z:zs) _ = [y * z] ++ (neuron_layer_interaction (y:ys) (zs) (z:zs))
neuron_layer_interaction (y:ys) [] (z:zs)= neuron_layer_interaction (ys) (z:zs) (z:zs)
--(1 - 8 - 1) neural network example without adjusting weights
neural_network_example:: [Float]

neural_network_example = neuron_layer 1 (neuron_layer_interaction (neuron_layer 8 (weights 24) 24) (neuron_layer_interaction (neuron_layer 1 (weights 3) 3) [] []) []) 3
-- adjusting weights for each layer
