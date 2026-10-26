/**
 * @file locations.stan
 * @brief Spatial hierarchy structure and precomputed nullspace QR basis.
 */

// Total location entities across all layers (minimum 3: 1 root + >= 2 children)
int<lower=3> n_locs;

// Maximum hierarchy depth (minimum 2 layers for hierarchical models)
int<lower=2> n_layers;

// 1-based start indices for each layer in depth order (layer 1 starts at 1)
array[n_layers] int<lower=1, upper=n_locs> layer_starts;

// Count of non-leaf parent entities having >= 2 offspring
int<lower=1> n_parent_locs;

// 1-based canonical location IDs of parent entities
array[n_parent_locs] int<lower=1, upper=n_locs> parent_loc_id;

// 1-based start index of contiguous children for each parent
array[n_parent_locs] int<lower=1, upper=n_locs> parent_child_starts;

// Location populations (accumulated bottom-up so parent population = sum of child populations)
vector<lower=0>[n_locs] loc_population;

// Total non-zero entries in block-diagonal nullspace QR basis
int<lower=1> n_qr_entries;

// [start, end] indices into unconstrained standard normal parameters z for parent p
array[2, n_parent_locs] int z_bounds;

// [start, end] indices into flattened qr_entries for parent p
array[2, n_parent_locs] int qr_bounds;

// Flattened columns of orthonormal nullspace basis Q_2 satisfying 1' * Q_2 = 0
vector[n_qr_entries] qr_entries;
