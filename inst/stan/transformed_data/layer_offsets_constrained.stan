// Transformed data indexing scheme for constrained (balanced) layer offsets:
// Maps constrained parameter draws (size (n_locs - 1) - n_parent_locs) to their 
// respective layer standard deviation parameter (sigma_layer, size n_layers - 1) 
// and parent location groups.

int n_constrained_draws = (n_locs - 1) - n_parent_locs;

// 1. Elementwise lookup: maps constrained draw index i (1 .. n_constrained_draws) to layer index k (1 .. n_layers - 1)
array[n_constrained_draws] int<lower=1, upper=n_layers - 1> draw_layer_map;

// 2. Layer range lookup: stores [start_index, end_index] in off_layer for each layer k (1 .. n_layers - 1)
array[2, n_layers - 1] int layer_draw_map;

// 3. Parent group free-draw range: stores [start_index, end_index] in off_layer for each parent p (1 .. n_parent_locs)
array[2, n_parent_locs] int parent_free_draw_map;

// 4. Parent group full offspring range: stores [start_index, end_index] in non-root location space (1 .. n_locs - 1) for parent p
array[2, n_parent_locs] int parent_full_loc_map;

// 5. Packed linear vector & metadata for precomputed orthonormal matrices Q_p
int total_Q_elements = 0;
array[n_parent_locs] int parent_Q_start;
array[n_parent_locs] int parent_Q_size;

{
  array[2, n_parent_locs] int parent_loc_ranges = bounds_to_range(layer_bounds, n_locs);
  for (p in 1:n_parent_locs) {
    int full_start = parent_loc_ranges[1, p] - 1;
    int full_end   = parent_loc_ranges[2, p] - 1;
    int num_children = full_end - full_start + 1;
    int num_free = num_children - 1;
    int q_size = num_children * num_free;

    parent_Q_start[p] = total_Q_elements + 1;
    parent_Q_size[p] = q_size;
    total_Q_elements += q_size;
  }
}

vector[total_Q_elements] Q_flat;

// Data containers for map_rect parallel execution across CPU threads
vector[0] phi_placeholder;
array[n_parent_locs, 1] int int_data_parent;
int max_q_size = max(parent_Q_size) > 0 ? max(parent_Q_size) : 1;
array[n_parent_locs, max_q_size] real real_data_parent;

{
  // Convert layer_bounds (in 1..n_locs space) to ranges for each parent location
  array[2, n_parent_locs] int parent_loc_ranges = bounds_to_range(layer_bounds, n_locs);

  int draw_pos = 1;
  int current_parent = 1;

  for (k in 1:(n_layers - 1)) {
    int layer_start_draw = draw_pos;
    int layer_nodes = layer_sizes[k];
    int nodes_processed = 0;

    // Process parent groups belonging to this layer
    while (nodes_processed < layer_nodes && current_parent <= n_parent_locs) {
      // Convert 1..n_locs location indices to 1..n_locs-1 non-root indices
      int full_start = parent_loc_ranges[1, current_parent] - 1;
      int full_end   = parent_loc_ranges[2, current_parent] - 1;
      int num_children = full_end - full_start + 1;
      int num_free = num_children - 1;

      parent_full_loc_map[1, current_parent] = full_start;
      parent_full_loc_map[2, current_parent] = full_end;

      int_data_parent[current_parent, 1] = num_children;

      if (num_free > 0) {
        parent_free_draw_map[1, current_parent] = draw_pos;
        parent_free_draw_map[2, current_parent] = draw_pos + num_free - 1;
        for (j in 1:num_free) {
          draw_layer_map[draw_pos] = k;
          draw_pos += 1;
        }

        matrix[num_children, num_free] Q_p = build_zero_sum_matrix(num_children);
        vector[num_children * num_free] q_vec = to_vector(Q_p);

        int q_st = parent_Q_start[current_parent];
        int q_sz = parent_Q_size[current_parent];
        Q_flat[q_st:(q_st + q_sz - 1)] = q_vec;

        real_data_parent[current_parent, 1:q_sz] = to_array_1d(q_vec);
        if (q_sz < max_q_size) {
          real_data_parent[current_parent, (q_sz + 1):max_q_size] = rep_array(0.0, max_q_size - q_sz);
        }
      } else {
        // Parent with 1 child has 0 free draws
        parent_free_draw_map[1, current_parent] = draw_pos;
        parent_free_draw_map[2, current_parent] = draw_pos - 1;

        real_data_parent[current_parent] = rep_array(0.0, max_q_size);
      }

      nodes_processed += num_children;
      current_parent += 1;
    }

    layer_draw_map[1, k] = layer_start_draw;
    layer_draw_map[2, k] = draw_pos - 1;
  }
}
