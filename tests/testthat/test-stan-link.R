target_logit <- "functions/link/logit.stan"
target_probit <- "functions/link/probit.stan"

skip_if_not_installed("rstan")
skip_if_stan_unchanged(target_logit)

model_logit <- sprintf(
  "
functions {
  #include %s
}
data {
  int N;
  vector[N] x_val;
  vector[N] p_val;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[N] out_link_fn = link_fn(p_val);
  vector[N] out_inv_link = inv_link(x_val);
  vector[N] out_d_inv_link = d_inv_link(x_val, inv_link(x_val));
  vector[N] out_d2_inv_link = d2_inv_link(x_val, inv_link(x_val), out_d_inv_link);
}
",
  target_logit
) |>
  compile_stan_harness()

model_probit <- sprintf(
  "
functions {
  #include %s
}
data {
  int N;
  vector[N] x_val;
  vector[N] p_val;
}
parameters {
  real dummy;
}
model {
  dummy ~ normal(0, 1);
}
generated quantities {
  vector[N] out_link_fn = link_fn(p_val);
  vector[N] out_inv_link = inv_link(x_val);
  vector[N] out_d_inv_link = d_inv_link(x_val, inv_link(x_val));
  vector[N] out_d2_inv_link = d2_inv_link(x_val, inv_link(x_val), out_d_inv_link);
}
",
  target_probit
) |>
  compile_stan_harness()

test_that("logit link and derivatives match analytical formulas", {
  x <- c(-3.0, -1.5, 0.0, 1.5, 3.0)
  p <- c(0.05, 0.2, 0.5, 0.8, 0.95)
  dat <- list(N = length(x), x_val = x, p_val = p)

  out_link <- run_stan_harness(model_logit, data = dat, out_link_fn)
  expect_equal(as.numeric(out_link), stats::qlogis(p), tolerance = 1e-10)

  out_inv <- run_stan_harness(model_logit, data = dat, out_inv_link)
  expect_equal(as.numeric(out_inv), stats::plogis(x), tolerance = 1e-10)

  out_d1 <- run_stan_harness(model_logit, data = dat, out_d_inv_link)
  p_x <- stats::plogis(x)
  expect_equal(as.numeric(out_d1), p_x * (1 - p_x), tolerance = 1e-10)

  out_d2 <- run_stan_harness(model_logit, data = dat, out_d2_inv_link)
  expect_equal(
    as.numeric(out_d2),
    (p_x * (1 - p_x)) * (1 - 2 * p_x),
    tolerance = 1e-10
  )
})

test_that("probit link and derivatives match analytical formulas", {
  x <- c(-2.5, -1.0, 0.0, 1.0, 2.5)
  p <- c(0.05, 0.2, 0.5, 0.8, 0.95)
  dat <- list(N = length(x), x_val = x, p_val = p)

  out_link <- run_stan_harness(model_probit, data = dat, out_link_fn)
  expect_equal(as.numeric(out_link), stats::qnorm(p), tolerance = 1e-10)

  out_inv <- run_stan_harness(model_probit, data = dat, out_inv_link)
  expect_equal(as.numeric(out_inv), stats::pnorm(x), tolerance = 1e-10)

  out_d1 <- run_stan_harness(model_probit, data = dat, out_d_inv_link)
  expect_equal(as.numeric(out_d1), stats::dnorm(x), tolerance = 1e-10)

  out_d2 <- run_stan_harness(model_probit, data = dat, out_d2_inv_link)
  expect_equal(as.numeric(out_d2), -x * stats::dnorm(x), tolerance = 1e-10)
})
