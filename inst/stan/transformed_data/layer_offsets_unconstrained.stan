// Transformed data indexing scheme for unconstrained layer offsets:
// Maps unconstrained parameter draws (size n_locs - 1) to their respective 
// layer standard deviation parameter (sigma_layer, size n_layers - 1).

// 1. Elementwise lookup: maps draw index i (1 .. n_locs - 1) to layer index k (1 .. n_layers - 1)
array[n_locs - 1] int<lower=1, upper=n_layers - 1> draw_layer_map;

// 2. Range lookup: stores [start_index, end_index] in off_layer for each layer k (1 .. n_layers - 1)
array[2, n_layers - 1] int layer_draw_map;

{
  int pos = 1;
  for (k in 1:(n_layers - 1)) {
    int layer_len = layer_sizes[k];
    layer_draw_map[1, k] = pos;
    layer_draw_map[2, k] = pos + layer_len - 1;
    for (j in 1:layer_len) {
      draw_layer_map[pos] = k;
      pos += 1;
    }
  }
}
