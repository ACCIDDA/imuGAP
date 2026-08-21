// Offsets - layer standard deviations
sigma_layer ~ cauchy(0, 1);

// Draw unconstrained layer offsets for each layer slice using layer_draw_map
for (k in 1:(n_layers - 1)) {
  off_layer[layer_draw_map[1, k]:layer_draw_map[2, k]] ~ normal(0, sigma_layer[k]);
}
