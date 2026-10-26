
/**
 * @file layer_offsets.stan
 * @brief Parameters for hierarchical random walk spatial offsets.
 */

// Standard deviation of spatial deviations per hierarchy layer (excluding root layer 1)
vector<lower=0>[n_layers - 1] sigma_layer;

// Unconstrained standard normal parameters projected into nullspace via Q_2
vector[n_unconstrained_offsets] z_layer;
