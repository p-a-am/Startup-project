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
--Complete neuron with adjustable wights
complete_neuron_rework:: [Float] -> Int -> Float

complete_neuron_rework y z = activation_function (sum_neuron y (num_of_parameters z))
--complete neuron layer with n neurons y to ys weights and z parameters
neuron_layer:: Int -> [Float] -> Int -> [Float]

neuron_layer n (y:ys) z
    |n > 0 = [complete_neuron y z] ++ neuron_layer (n-1) ys z
    |n == 0 = []

--interaction between neuron layers
neuron_layer_interaction::[Float] -> [Float] -> [Float]-> [Float]

neuron_layer_interaction (y:ys) (z:zs) _ = [y * z] ++ (neuron_layer_interaction (y:ys) (zs) (z:zs))
neuron_layer_interaction (y:ys) [] (z:zs)= neuron_layer_interaction (ys) (z:zs) (z:zs)
neuron_layer_interaction [] _ _ = []
--(1 - 8 - 1) neural network example without adjusting weights
neural_network_example:: [Float]

neural_network_example = neuron_layer 1 (neuron_layer_interaction (neuron_layer 8 (weights 24) 24) (neuron_layer_interaction (neuron_layer 1 (weights 3) 3) [] []) []) 3
-- memorizing and adjusting weights for each neuron
adjusting_weights_for_each_neuron:: [Float] -> [Float] -> [Int] -> [Float]

adjusting_weights_for_each_neuron (saida:xs) (esperado:ys) (variavel:zs) = [2 * (saida - esperado) - (int_to_float variavel)] ++ adjusting_weights_for_each_neuron (saida:xs) (esperado : ys) zs
adjusting_weights_for_each_neuron (saida:xs) (esperado:ys) [] = []
adjusting_weights_for_each_neuron (saida:xs) [] (variavel:zs) = (saida:xs)
--Neural network reworking the neuron building
nn_neuron_building :: [Float] -> [Float] -> Int  -> [Float]

nn_neuron_building value (weight:xs) param = adjusting_weights_for_each_neuron [complete_neuron_rework (weight:xs) (param)] value (num_of_parameters param)
--Neural network reworking the layer where weight is a 2D Matrix of weights for each layer
nn_layers :: [Float] ->[[Float]] -> Int -> [Float]

nn_layers value ((x:xs):ys) param = nn_neuron_building value (x:xs) param ++ nn_layers value ys param
nn_layers value [] param = []
--creating a 2D matrix of random weights

matrix_random_weights:: Int -> Int -> [[Float]]
matrix_random_weights x y = [weights x] ++ matrix_random_weights (x-0.004387) (y-1)
--rebuilding and the NN itself (1 - 8 - 1) with random weights and random input

neural_network_example_2:: [Float]
neural_network_example_2 = nn_layers (nn_layers (nn_layers (nn_layers [] [] 0) (matrix_random_weights 3 1) 1) (matrix_random_weights 24 8) 8) (matrix_random_weights 3 1) 1
