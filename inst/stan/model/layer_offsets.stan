
    // Offsets - layer standard deviations and unconstrained layer offsets
    sigma_layer ~ cauchy(0, sigma_layer_scale);
    off_layer ~ normal(0, sigma_layer[loc_layer_idx]);
