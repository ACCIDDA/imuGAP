skip_if_not_installed("rstan")
# ' "transformed_data/layer_indices.stan" defines
# ' precomputed layer indexing structures (`layer_bounds`, `parent_child_bounds`,
# ' `loc_layer_idx`) for hierarchical multi-layer spatial models.

target <- "transformed_data/layer_indices.stan"

skip_if_stan_unchanged(c(
  "functions/bounds_to_range.stan",
  "functions/layer_offsets.stan",
  target
))

model_layer_indices <- sprintf(
  "
functions {
  #include functions/bounds_to_range.stan
  #include functions/layer_offsets.stan
}
data {
  int n_locs;
  int n_layers;
  array[n_layers] int layer_starts;
  int n_parent_locs;
  array[n_parent_locs] int parent_loc_id;
  array[n_parent_locs] int parent_child_starts;
  vector<lower=0>[n_locs] loc_population;
}
transformed data {
  #include %s
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  array[2, n_layers] int out_layer_bounds = layer_bounds;
  array[2, n_parent_locs] int out_parent_child_bounds = parent_child_bounds;
  array[n_locs - 1] int out_loc_layer_idx = loc_layer_idx;
  int out_n_unconstrained_offsets = n_unconstrained_offsets;
  int out_n_qr_entries = n_qr_entries;
  array[2, n_parent_locs] int out_z_bounds = z_bounds;
  array[2, n_parent_locs] int out_qr_bounds = qr_bounds;
  vector[n_qr_entries] out_qr_entries = qr_entries;
  vector[n_locs - 1] out_loc_pop_scale = loc_pop_scale;
}
",
  target
) |>
  compile_stan_harness()

test_that("layer_indices.stan constructs multi-layer mappings with canonical hierarchy", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- imuGAP:::canonicalize_locations(locations_sim)
  ld_sim <- imuGAP:::assemble_layer_data(locs_sim)

  layer_bounds <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_layer_bounds
  )
  expect_equal(as.numeric(layer_bounds[1, ]), as.numeric(ld_sim$layer_starts))
  expect_equal(
    as.numeric(layer_bounds[2, ]),
    as.numeric(c(tail(ld_sim$layer_starts, -1) - 1L, ld_sim$n_locs))
  )

  parent_child_bounds <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_parent_child_bounds
  )
  expect_equal(
    as.numeric(parent_child_bounds[1, ]),
    as.numeric(ld_sim$parent_child_starts)
  )
  expect_equal(
    as.numeric(parent_child_bounds[2, ]),
    as.numeric(c(tail(ld_sim$parent_child_starts, -1) - 1L, ld_sim$n_locs))
  )

  loc_layer_idx <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_loc_layer_idx
  )
  layer_sizes <- c(
    diff(ld_sim$layer_starts),
    ld_sim$n_locs - tail(ld_sim$layer_starts, 1) + 1L
  )
  expect_equal(
    as.numeric(loc_layer_idx),
    c(rep(1L, layer_sizes[2]), rep(2L, layer_sizes[3]))
  )

  n_unconstrained <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_n_unconstrained_offsets
  )
  expect_equal(n_unconstrained, (ld_sim$n_locs - 1L) - ld_sim$n_parent_locs)

  z_bounds <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_z_bounds
  )
  qr_bounds <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_qr_bounds
  )
  qr_entries <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_qr_entries
  )

  loc_pop_scale <- run_stan_harness(
    model_layer_indices,
    data = ld_sim,
    out_loc_pop_scale
  )
  expect_length(loc_pop_scale, ld_sim$n_locs - 1L)
  expect_true(all(loc_pop_scale > 0))

  # Verify layer-wide scaling
  for (k in seq_len(ld_sim$n_layers - 1L)) {
    st <- layer_bounds[1, k + 1L]
    en <- layer_bounds[2, k + 1L]
    layer_pop <- ld_sim$loc_population[st:en]
    expected_scale <- as.numeric(sqrt(mean(layer_pop) / layer_pop))
    expect_equal(
      as.numeric(loc_pop_scale[(st - 1L):(en - 1L)]),
      expected_scale,
      tolerance = 1e-6
    )
  }

  # 1. Verify orthonormal nullspace basis properties for every parent block
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- parent_child_bounds[1, p]
    en <- parent_child_bounds[2, p]
    k_len <- en - st + 1L
    q_st <- qr_bounds[1, p]
    q_en <- qr_bounds[2, p]
    q_star <- matrix(qr_entries[q_st:q_en], nrow = k_len, ncol = k_len - 1L)

    # Orthonormality: Q*^T Q* = I
    expect_equal(t(q_star) %*% q_star, diag(k_len - 1L), tolerance = 1e-6)

    # Orthogonality to sqrt(population weights): v1^T Q* = 0
    pop_slice <- ld_sim$loc_population[st:en]
    w_sqrt <- sqrt(pop_slice / sum(pop_slice))
    expect_equal(
      as.numeric(t(q_star) %*% w_sqrt),
      rep(0, k_len - 1L),
      tolerance = 1e-6
    )
  }

  # 2. Compare block QR transformation directly against full-matrix formulation
  qr_basis_dense <- matrix(0, nrow = ld_sim$n_locs - 1L, ncol = n_unconstrained)
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- parent_child_bounds[1, p]
    en <- parent_child_bounds[2, p]
    k_len <- en - st + 1L
    z_st <- z_bounds[1, p]
    z_en <- z_bounds[2, p]
    q_st <- qr_bounds[1, p]
    q_en <- qr_bounds[2, p]
    q_star <- matrix(qr_entries[q_st:q_en], nrow = k_len, ncol = k_len - 1L)
    qr_basis_dense[(st - 1L):(en - 1L), z_st:z_en] <- q_star
  }

  set.seed(123)
  z_draw <- rnorm(n_unconstrained)
  raw_off_stan <- numeric(ld_sim$n_locs - 1L)
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- parent_child_bounds[1, p]
    en <- parent_child_bounds[2, p]
    k_len <- en - st + 1L
    z_st <- z_bounds[1, p]
    z_en <- z_bounds[2, p]
    q_st <- qr_bounds[1, p]
    q_en <- qr_bounds[2, p]
    q_star <- matrix(qr_entries[q_st:q_en], nrow = k_len, ncol = k_len - 1L)
    raw_off_stan[(st - 1L):(en - 1L)] <- as.vector(q_star %*% z_draw[z_st:z_en])
  }

  # Confirm exact equality with original full-matrix multiplication
  expect_equal(
    raw_off_stan,
    as.vector(qr_basis_dense %*% z_draw),
    tolerance = 1e-6
  )

  # Verify per-parent balanced delta: sum_{i in children(p)} N_i * off_layer_i == 0
  off_layer <- raw_off_stan * loc_pop_scale
  for (p in seq_len(ld_sim$n_parent_locs)) {
    st <- parent_child_bounds[1, p]
    en <- parent_child_bounds[2, p]
    child_pops <- ld_sim$loc_population[st:en]
    child_offsets <- off_layer[(st - 1L):(en - 1L)]
    weighted_sum <- sum(child_pops * child_offsets)
    expect_equal(weighted_sum, 0, tolerance = 1e-6)
  }
})
