
  #include data/shared_single.stan

  // Location hierarchy: arbitrary user-specified layers (multi-layer, n_layers >= 2)
  int<lower=3> n_locs; // total number of locations (root + all sub-locations)
  int<lower=2> n_layers; // number of hierarchy layers (depth >= 2)
  array[n_layers] int<lower=1> layer_sizes; // number of locations in each layer
  array[2, n_layers] int<lower=1> layer_bounds; // start and end location indices for each layer
  array[n_locs] int<lower=0> parent_id_map; // parent location index (0 for root location)
  array[n_locs] int<lower=1> layer_id_map; // layer index (1..n_layers) for each location
  int<lower=1> n_parent_locs; // number of parent locations with children
  array[n_parent_locs] int<lower=1, upper=n_locs> parent_loc_id; // canonical loc ID of each parent
  array[2, n_parent_locs] int<lower=1, upper=n_locs> parent_child_bounds; // [start_child_id, end_child_id] for each parent

  // Location indices for weights
  array[n_weights_uncensored] int<lower=1, upper=n_locs> weights_location_uncensored;
  array[n_weights_right] int<lower=1, upper=n_locs> weights_location_right;
  array[n_weights_left] int<lower=1, upper=n_locs> weights_location_left;
