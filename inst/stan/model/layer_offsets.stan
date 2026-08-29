
    // Offsets - layer standard deviations and unconstrained layer offsets
    sigma_layer ~ cauchy(0, 1);
    off_layer ~ normal(0, sigma_layer[loc_layer_idx]);
