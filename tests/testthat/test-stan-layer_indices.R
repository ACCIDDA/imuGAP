skip_if_not_installed("rstan")
# ' "transformed_data/layer_indices.stan" defines
# ' precomputed layer indexing structures (`layer_bounds`, `parent_child_bounds`,
# ' `loc_layer_idx`) for hierarchical multi-layer spatial models.

target <- "transformed_data/layer_indices.stan"

skip_if_stan_unchanged(c(
  "functions/bounds_to_range.stan",
  target
))

model_layer_indices <- sprintf(
  "
functions {
  #include functions/bounds_to_range.stan
}
data {
  int n_locs;
  int n_layers;
  array[n_layers] int layer_starts;
  int n_parent_locs;
  array[n_parent_locs] int parent_loc_id;
  array[n_parent_locs] int parent_child_starts;
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
}
",
  target
) |>
  compile_stan_harness()

test_that("layer_indices.stan constructs multi-layer mappings with canonical hierarchy", {
  data("locations_sim", package = "imuGAP")
  locs_sim <- canonicalize_locations(locations_sim)
  ld_sim <- assemble_layer_data(locs_sim)

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
})
