// Layer standard deviations (one for each non-root layer, i.e., n_layers - 1)
vector<lower=0>[n_layers - 1] sigma_layer;

// Layer offset parameter vector across all non-root locations (size n_locs - 1):
// - Independent of constrained vs. unconstrained model formulation.
// - In unconstrained models: drawn directly from normal(0, sigma_layer).
// - In constrained models: computed in model block from unconstrained local draws.
row_vector[n_locs - 1] off_layer;

