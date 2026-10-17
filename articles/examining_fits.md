# Fit Inspection and Stan Diagnostics

## Introduction

Because `imuGAP` estimates an underlying process model for vaccination
(including baseline uptake splines $`\beta_{\text{bs}}`$,
force-of-vaccination rates $`\lambda`$, and hierarchical location
offsets $`\delta`$) via Bayesian inference in Stan, evaluating a model
fit entails assessing whether MCMC chains converged without pathologies.
Ideally, you would also synthesize data in a way you think is
representative of your system and attempt to recover the latent process
parameters. The example data in the package demonstrates that workflow.

This vignette demonstrates how to:

1.  Load and extract posterior parameter draws from an `imugap_fit`
    object using
    [`extract_imugap()`](https://accidda.github.io/imuGAP/reference/extract_imugap.md).
2.  Inspect parameter trace plots across multiple chains.
3.  Compare posterior parameter estimates against true data-generating
    simulation values (`latent_params_sim`).
4.  Evaluate spatial variance components ($`\sigma`$) and
    location-specific random offsets ($`\delta`$).

------------------------------------------------------------------------

## 1. Loading the Model Fit and Extracting Parameters

You can load the bundled `fit_sim` object, which wraps the underlying
Stan model fit along with dataset and options metadata:

``` r

data("fit_sim", package = "imuGAP")
data("latent_params_sim", package = "imuGAP")
data("locations_sim", package = "imuGAP")
data("observations_sim", package = "imuGAP")

fit_sim
#> An imuGAP model fit (`imugap_fit`):
#>   Hierarchy:    28 locations across 3 layers (root: 'State')
#>   Observations: 841 total (775 uncensored, 66 right-censored)
#> 
#> Inference for Stan model: impute_school_coverage_process_v6.
#> 4 chains, each with iter=1000; warmup=500; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=2000.
#> 
#>                       mean se_mean   sd      2.5%       25%       50%       75%
#> beta_bs[1]           -1.68    0.00 0.03     -1.74     -1.70     -1.68     -1.65
#> beta_bs[2]           -1.88    0.00 0.05     -1.97     -1.91     -1.88     -1.84
#> beta_bs[3]           -2.48    0.00 0.08     -2.65     -2.54     -2.48     -2.43
#> beta_bs[4]           -3.12    0.00 0.09     -3.30     -3.18     -3.12     -3.06
#> beta_bs[5]           -2.59    0.00 0.09     -2.77     -2.65     -2.59     -2.53
#> sigma_layer[1]        0.53    0.01 0.29      0.18      0.31      0.45      0.66
#> sigma_layer[2]        0.75    0.01 0.13      0.54      0.66      0.74      0.83
#> lambda_raw[1]         1.02    0.00 0.03      0.97      1.00      1.02      1.04
#> lambda_raw[2]         1.06    0.00 0.01      1.03      1.05      1.06      1.07
#> raw_phi_root[1]      -1.68    0.00 0.03     -1.74     -1.70     -1.68     -1.65
#> raw_phi_root[2]      -1.72    0.00 0.03     -1.77     -1.74     -1.72     -1.70
#> raw_phi_root[3]      -1.77    0.00 0.02     -1.81     -1.78     -1.77     -1.75
#> raw_phi_root[4]      -1.81    0.00 0.02     -1.86     -1.83     -1.81     -1.80
#> raw_phi_root[5]      -1.86    0.00 0.03     -1.91     -1.88     -1.86     -1.85
#> raw_phi_root[6]      -1.92    0.00 0.03     -1.97     -1.93     -1.92     -1.90
#> raw_phi_root[7]      -1.97    0.00 0.03     -2.02     -1.99     -1.97     -1.95
#> raw_phi_root[8]      -2.03    0.00 0.03     -2.08     -2.05     -2.03     -2.01
#> raw_phi_root[9]      -2.09    0.00 0.03     -2.14     -2.10     -2.09     -2.07
#> raw_phi_root[10]     -2.15    0.00 0.03     -2.20     -2.16     -2.15     -2.13
#> raw_phi_root[11]     -2.21    0.00 0.03     -2.26     -2.23     -2.21     -2.19
#> raw_phi_root[12]     -2.27    0.00 0.03     -2.33     -2.29     -2.27     -2.25
#> raw_phi_root[13]     -2.33    0.00 0.03     -2.40     -2.35     -2.33     -2.31
#> raw_phi_root[14]     -2.39    0.00 0.03     -2.46     -2.42     -2.39     -2.37
#> raw_phi_root[15]     -2.46    0.00 0.04     -2.53     -2.48     -2.46     -2.43
#> raw_phi_root[16]     -2.52    0.00 0.04     -2.60     -2.55     -2.52     -2.50
#> raw_phi_root[17]     -2.59    0.00 0.04     -2.66     -2.61     -2.59     -2.56
#> raw_phi_root[18]     -2.65    0.00 0.04     -2.73     -2.67     -2.65     -2.62
#> raw_phi_root[19]     -2.71    0.00 0.04     -2.79     -2.73     -2.70     -2.68
#> raw_phi_root[20]     -2.76    0.00 0.04     -2.85     -2.79     -2.76     -2.73
#> raw_phi_root[21]     -2.80    0.00 0.05     -2.90     -2.83     -2.80     -2.77
#> raw_phi_root[22]     -2.84    0.00 0.05     -2.93     -2.87     -2.84     -2.80
#> raw_phi_root[23]     -2.86    0.00 0.05     -2.96     -2.89     -2.86     -2.83
#> raw_phi_root[24]     -2.87    0.00 0.05     -2.97     -2.91     -2.87     -2.84
#> raw_phi_root[25]     -2.87    0.00 0.05     -2.97     -2.90     -2.87     -2.84
#> raw_phi_root[26]     -2.85    0.00 0.05     -2.95     -2.89     -2.85     -2.82
#> raw_phi_root[27]     -2.82    0.00 0.05     -2.92     -2.85     -2.82     -2.79
#> raw_phi_root[28]     -2.76    0.00 0.05     -2.88     -2.80     -2.76     -2.73
#> raw_phi_root[29]     -2.69    0.00 0.07     -2.83     -2.73     -2.69     -2.64
#> raw_phi_root[30]     -2.59    0.00 0.09     -2.77     -2.65     -2.59     -2.53
#> lp__             -78864.90    0.32 5.28 -78875.91 -78868.14 -78864.46 -78861.20
#>                      97.5% n_eff Rhat
#> beta_bs[1]           -1.62  1271 1.00
#> beta_bs[2]           -1.78  1000 1.00
#> beta_bs[3]           -2.32   917 1.00
#> beta_bs[4]           -2.95   998 1.00
#> beta_bs[5]           -2.43   961 1.00
#> sigma_layer[1]        1.30   615 1.02
#> sigma_layer[2]        1.04   164 1.03
#> lambda_raw[1]         1.07  1227 1.00
#> lambda_raw[2]         1.09  1073 1.00
#> raw_phi_root[1]      -1.62  1271 1.00
#> raw_phi_root[2]      -1.67  1288 1.00
#> raw_phi_root[3]      -1.72  1229 1.00
#> raw_phi_root[4]      -1.77  1148 1.00
#> raw_phi_root[5]      -1.81  1110 1.00
#> raw_phi_root[6]      -1.87  1111 1.00
#> raw_phi_root[7]      -1.92  1135 1.00
#> raw_phi_root[8]      -1.98  1165 1.00
#> raw_phi_root[9]      -2.03  1186 1.00
#> raw_phi_root[10]     -2.09  1187 1.00
#> raw_phi_root[11]     -2.15  1171 1.00
#> raw_phi_root[12]     -2.21  1147 1.00
#> raw_phi_root[13]     -2.27  1123 1.00
#> raw_phi_root[14]     -2.33  1108 1.00
#> raw_phi_root[15]     -2.39  1113 1.00
#> raw_phi_root[16]     -2.45  1119 1.00
#> raw_phi_root[17]     -2.51  1116 1.00
#> raw_phi_root[18]     -2.57  1112 1.00
#> raw_phi_root[19]     -2.62  1104 1.00
#> raw_phi_root[20]     -2.67  1091 1.00
#> raw_phi_root[21]     -2.71  1077 1.00
#> raw_phi_root[22]     -2.74  1066 1.00
#> raw_phi_root[23]     -2.77  1060 1.00
#> raw_phi_root[24]     -2.78  1062 1.00
#> raw_phi_root[25]     -2.78  1079 1.00
#> raw_phi_root[26]     -2.76  1116 1.00
#> raw_phi_root[27]     -2.72  1173 1.00
#> raw_phi_root[28]     -2.66  1072 1.00
#> raw_phi_root[29]     -2.57  1002 1.00
#> raw_phi_root[30]     -2.43   961 1.00
#> lp__             -78855.71   278 1.03
#> 
#> Samples were drawn using NUTS(diag_e) at Sat Oct 17 13:43:03 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```

To extract posterior draws for specific model parameters, use
[`extract_imugap()`](https://accidda.github.io/imuGAP/reference/extract_imugap.md):

``` r

# Extract B-spline basis coefficients
beta_draws <- extract_imugap(fit_sim, pars = "beta_bs")
str(beta_draws)
#> List of 1
#>  $ beta_bs: num [1:2000, 1:5] -1.65 -1.65 -1.62 -1.67 -1.7 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ iterations: NULL
#>   .. ..$           : NULL

# Extract unconstrained force of vaccination rate parameters
lambda_draws <- extract_imugap(fit_sim, pars = "lambda_raw")
str(lambda_draws)
#> List of 1
#>  $ lambda_raw: num [1:2000, 1:2] 1.04 1.04 1.01 1.05 1.05 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ iterations: NULL
#>   .. ..$           : NULL
```

------------------------------------------------------------------------

## 2. Visual Diagnostics with `bayesplot` and Parameter Recovery

#### State-Level Cohort Uptake: Basis Spline Coefficients ($`\beta_{\text{bs}}`$)

The state-level baseline propensity curve is parameterized as a B-spline
over birth cohorts with coefficients $`\beta_{\text{bs}}`$. The
following trace plots show parameter draws across all 4 MCMC chains
alongside within-chain medians and the true simulation parameters
(dashed red lines):

**Show plot code**

``` r

beta_pars <- paste0("beta_bs[", seq_along(latent_params_sim$beta_bs), "]")
beta_arr <- as.array(fit_sim$raw_fit, pars = beta_pars)
beta_chain_meds <- rbindlist(lapply(beta_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(beta_arr)[2])),
    med_val = apply(beta_arr[, , p, drop = FALSE], 2, median)
  )
}))

beta_ref <- data.frame(
  parameter = beta_pars,
  true_val = latent_params_sim$beta_bs,
  label = sprintf(
    "True~beta[%d] == %.2f",
    seq_along(latent_params_sim$beta_bs),
    latent_params_sim$beta_bs
  )
)

bayesplot::mcmc_trace(
  beta_arr,
  facet_args = list(labeller = ggplot2::as_labeller(function(x) {
    gsub("beta_bs\\[(\\d+)\\]", "beta[\\1]", x)
  }, default = ggplot2::label_parsed))
) +
  geom_hline(
    data = beta_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = beta_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_label(
    data = beta_ref,
    aes(x = 100, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-beta-1.png)

------------------------------------------------------------------------

#### Hierarchy Layer Variances ($`\sigma_{\text{layer}}`$)

In multi-layer models, `sigma_layer` captures the standard deviation of
location random offsets at each level of the tree
(e.g. $`\sigma_{\text{county}}`$ at layer 1 and
$`\sigma_{\text{school}}`$ at layer 2):

**Show plot code**

``` r

sigma_pars <- c("sigma_layer[1]", "sigma_layer[2]")
sigma_arr <- as.array(fit_sim$raw_fit, pars = sigma_pars)
sigma_chain_meds <- rbindlist(lapply(sigma_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(sigma_arr)[2])),
    med_val = apply(sigma_arr[, , p, drop = FALSE], 2, median)
  )
}))

sigma_ref <- data.frame(
  parameter = sigma_pars,
  true_val = c(latent_params_sim$sigma_cnty, latent_params_sim$sigma_sch),
  label = sprintf(
    "True~sigma == %.2f",
    c(latent_params_sim$sigma_cnty, latent_params_sim$sigma_sch)
  )
)

bayesplot::mcmc_trace(
  sigma_arr,
  facet_args = list(labeller = ggplot2::as_labeller(c(
    "sigma_layer[1]" = "sigma[County]",
    "sigma_layer[2]" = "sigma[School]"
  ), default = ggplot2::label_parsed))
) +
  geom_hline(
    data = sigma_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = sigma_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_label(
    data = sigma_ref,
    aes(x = 100, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  coord_cartesian(ylim = c(0, 2.5)) +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-sigmas-1.png)

------------------------------------------------------------------------

#### County Location Offsets ($`\delta_{\text{county}}`$)

Location offsets represent deviation in vaccination propensity from the
parent region. The following trace plots show offset trajectories for
Scruggs, Simone, and Watson counties.

Indicator arrows show:

- **Red arrows (left edge, iteration 0)**: Expected error direction
  based on observational sampling noise from finite sample draws
  ($`\Delta_{\delta} = -\overline{\Delta\text{logit}}_{\text{cov}}`$).
- **Blue arrows (right edge, iteration 500)**: Realized difference
  between the posterior median estimate and the true latent offset.

**Show plot code**

``` r

loc_info <- canonicalize_locations(locations_sim)
off_layer_arr <- as.array(fit_sim$raw_fit, pars = "off_layer")

county_names <- names(latent_params_sim$off_cnty)
non_root_locs <- loc_info$loc_id[-1]
county_pars <- paste0("off_layer[", match(county_names, non_root_locs), "]")
county_arr <- off_layer_arr[, , county_pars, drop = FALSE]
county_chain_meds <- rbindlist(lapply(county_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(county_arr)[2])),
    med_val = apply(county_arr[, , p, drop = FALSE], 2, median)
  )
}))

obs <- copy(observations_sim)
phi_st <- latent_params_sim$phi_state
cov <- latent_params_sim$uptake
off_cnty <- latent_params_sim$off_cnty
censor_red <- latent_params_sim$censor_reduction

obs_cnty <- obs[loc_id %in% county_names]
obs_cnty[, mu := {
  c_off <- unname(off_cnty[loc_id])
  phi_c <- plogis(qlogis(phi_st[cohort]) + c_off)
  (1 - phi_c) * cov[11, 2] * censor_red
}, by = loc_id]
obs_cnty[, p_adj := (positive + 0.5) / (sample_n + 1.0)]
obs_cnty[, delta_cov := qlogis(p_adj) - qlogis(mu)]

cnty_errs <- obs_cnty[, .(expected_delta_err = -mean(delta_cov)), by = .(loc_id)]
county_post_meds <- apply(county_arr, 3, median)

county_ref <- data.frame(
  parameter = county_pars,
  loc_id = county_names,
  true_val = unname(latent_params_sim$off_cnty[county_names]),
  post_med = unname(county_post_meds[county_pars]),
  label = sprintf(
    "True~delta == %.2f",
    latent_params_sim$off_cnty[county_names]
  )
)
county_ref <- merge(county_ref, cnty_errs, by = "loc_id")

county_ref$red_arrow_x <- 0
county_ref$red_arrow_yend <- county_ref$true_val + county_ref$expected_delta_err
county_ref$blue_arrow_x <- 500
county_ref$blue_arrow_yend <- county_ref$post_med

bayesplot::mcmc_trace(
  county_arr,
  facet_args = list(
    scales = "fixed",
    labeller = ggplot2::as_labeller(setNames(county_names, county_pars))
  )
) +
  geom_hline(
    data = county_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = county_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_segment(
    data = county_ref,
    aes(x = red_arrow_x, xend = red_arrow_x, y = true_val, yend = red_arrow_yend),
    arrow = arrow(length = unit(0.04, "inches"), type = "closed"),
    color = "firebrick", linewidth = 0.8
  ) +
  geom_segment(
    data = county_ref,
    aes(x = blue_arrow_x, xend = blue_arrow_x, y = true_val, yend = blue_arrow_yend),
    arrow = arrow(length = unit(0.04, "inches"), type = "closed"),
    color = "royalblue", linewidth = 0.8
  ) +
  geom_label(
    data = county_ref,
    aes(x = 50, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-county-offsets-1.png)

------------------------------------------------------------------------

#### Force of Vaccination Rate ($`\lambda`$)

The unconstrained rate parameters $`\lambda_{\text{raw}}`$ govern the
speed at which eligible individuals receive vaccine doses once reaching
age eligibility. The following trace plots show posterior draws on the
exponentiated rate scale:

**Show plot code**

``` r

lambda_pars <- c("lambda_raw[1]", "lambda_raw[2]")
lambda_arr <- as.array(fit_sim$raw_fit, pars = lambda_pars)
lambda_chain_meds <- rbindlist(lapply(lambda_pars, function(p) {
  data.table(
    parameter = p,
    Chain = factor(seq_len(dim(lambda_arr)[2])),
    med_val = apply(lambda_arr[, , p, drop = FALSE], 2, median)
  )
}))

lambda_ref <- data.frame(
  parameter = lambda_pars,
  true_val = log(latent_params_sim$lambda),
  label = sprintf("True~lambda == %.1f", latent_params_sim$lambda)
)

bayesplot::mcmc_trace(
  lambda_arr,
  facet_args = list(labeller = ggplot2::as_labeller(c(
    "lambda_raw[1]" = "lambda[1]~(Dose~1)",
    "lambda_raw[2]" = "lambda[2]~(Dose~2)"
  ), default = ggplot2::label_parsed))
) +
  geom_hline(
    data = lambda_chain_meds,
    aes(yintercept = med_val, color = Chain),
    linetype = "solid", linewidth = 0.5, alpha = 0.8
  ) +
  geom_hline(
    data = lambda_ref,
    aes(yintercept = true_val),
    color = "firebrick", linetype = "dashed", linewidth = 0.8
  ) +
  geom_label(
    data = lambda_ref,
    aes(x = 100, y = true_val, label = label),
    parse = TRUE, color = "firebrick", fill = ggplot2::alpha("white", 0.75),
    linewidth = NA, vjust = -0.3, hjust = 0, size = 3.2
  ) +
  coord_cartesian(ylim = c(0.5, 1.5)) +
  scale_y_continuous(
    transform = "exp",
    labels = function(x) sprintf("%.2f", exp(x))
  ) +
  labs(y = "Uptake Rate (exponential scale)") +
  theme(legend.position = "none")
```

![](examining_fits_files/figure-html/trace-plot-lambdas-1.png)

------------------------------------------------------------------------

## Summary and Related Vignettes

- For an overview of estimating the underlying process model and
  predicting coverage, see **[Getting Started with
  imuGAP](https://accidda.github.io/imuGAP/articles/imuGAP.md)**.
- To explore input data preparation and surveillance streams, see
  **[Included Example Datasets and Data
  Validation](https://accidda.github.io/imuGAP/articles/example_data.md)**.
- To learn how `imuGAP` estimates parameters across arbitrary spatial
  hierarchy resolutions, see **[Flexible Location Layers in
  imuGAP](https://accidda.github.io/imuGAP/articles/user_specified_layers.md)**.
