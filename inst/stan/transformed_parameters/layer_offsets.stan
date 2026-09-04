
  // Constrained layer offsets mapped through orthonormal QR basis and scaled by layer standard deviations
  vector[n_locs - 1] off_layer = (qr_basis * z_layer) .* sigma_layer[loc_layer_idx];
