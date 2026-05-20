# Regenerate the `fit_sim` package fixture.
#
# This script fits the imuGAP Stan model to the bundled `*_sim` datasets using
# the same minimal sampler settings as `tests/testthat/test-imugap-smoke.R`
# (1 chain, 100 iterations, seed 1). The resulting stanfit object is saved to
# `data/fit_sim.rda` so it can be reused by tests and examples without
# recompiling / refitting the model on every run.
#
# stanfit objects bundle references to the compiled Stan model and can be
# fragile across major `rstan` / `StanHeaders` updates. If `fit_sim` fails to
# load on a future install, regenerate it by running this script from the
# package root:
#
#     Rscript data-raw/fit_sim.R
#
# Empirical baseline (single chain, 100 iterations, after model compilation):
#  - runtime: ~10 seconds
#  - on-disk size with `compress = "xz"`: ~1 MB (well under CRAN's 5 MB limit)

pkgload::load_all(quiet = TRUE)

fit_sim <- suppressWarnings(imuGAP(
  observations_sim,
  populations_sim,
  locations_sim,
  stan_opts = stan_options(
    iter = 100,
    chains = 1,
    refresh = 0,
    seed = 1L
  )
))

usethis::use_data(fit_sim, compress = "xz", overwrite = TRUE)
