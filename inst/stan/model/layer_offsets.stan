
    // Offsets - layer standard deviations and unconstrained standard normal draws
    sigma_layer ~ cauchy(0, 1);
    z_layer ~ std_normal();
