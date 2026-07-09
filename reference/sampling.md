# Immunity: Geographic & Age-based Projection, `imuGAP`

This a sampler interface to convert user-friendly data into the
necessary format to feed the immunity estimation model.

## Usage

``` r
sampling(
  observations,
  populations,
  locations,
  imugap_opts = imugap_options(),
  stan_opts = stan_options()
)
```

## Arguments

- observations:

  a `[data.frame()]`, the observed data, with at least three columns:

  - an `obs_id` column; any type, as long as unique, non-NA

  - a `positive` column; non-negative integers, the observed number of
    vaccinated individuals

  - a `sample_n` column; positive integers, the number of individuals
    sampled, must be greater than or equal to "positive"

  - optionally, a `censored` column; numeric, NA (uncensored) or 1
    (right-censored); if not present, will be assumed NA

- populations:

  a `[data.frame()]`, the observation meta data, with columns

  - `obs_id`, any type; the observation the row concerns (i.e. id shared
    with an observations data object)

  - `loc_id`, any type; the location the row concerns (i.e. id shared
    with a locations data object)

  - `dose`, a non-zero, positive integer (1, 2, ...); what dose row
    concerns

  - `cohort`, a positive integer; the cohort at the location row
    concerns

  - `age`, a positive integer; the age of that cohort row concerns

  - `weight`, a numeric, (0, 1); the relative contribution of this row
    to an observation. Optional if each population row has a unique
    `obs_id`.

- locations:

  a `[data.frame()]`, with columns `loc_id` and `parent_id`, of the same
  type. See Details for restrictions.

- imugap_opts:

  options for the `imuGAP` model

- stan_opts:

  passed to
  [`rstan::sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  (e.g. `iter`, `chains`).

## Value

An object of class `imugap_fit` wrapping the raw `stanfit` object along
with settings and dataset metadata.

## Details

If the Stan sampler fails to initialize and produces no draws (for the
rstan backend, a mode-2 `stanfit` with an empty `@sim`), `sampling()`
raises an error of class `imugap_no_draws` rather than returning an
empty fit, so the failure can be handled with
[`tryCatch()`](https://rdrr.io/r/base/conditions.html). The check is
backend-agnostic (see
[`backend_has_draws()`](https://accidda.github.io/imuGAP/reference/backend_has_draws.md)).

## Examples

``` r
# \donttest{
data("locations_sim"); data("observations_sim"); data("populations_sim")
st_opts <- stan_options(chains = 2, iter = 500)
sampling(
  observations_sim, populations_sim, locations_sim,
  stan_opts = st_opts
)
#> 
#> SAMPLING FOR MODEL 'impute_school_coverage_process_v6' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0.000224 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.24 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 1: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 1: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 1: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 1: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 1: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 1: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 1: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 1: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 7.445 seconds (Warm-up)
#> Chain 1:                3.843 seconds (Sampling)
#> Chain 1:                11.288 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'impute_school_coverage_process_v6' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.000187 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.87 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 2: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 2: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 2: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 2: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 2: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 2: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 2: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 2: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 2: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 2: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 2: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 8.101 seconds (Warm-up)
#> Chain 2:                6.751 seconds (Sampling)
#> Chain 2:                14.852 seconds (Total)
#> Chain 2: 
#> Warning: There were 5 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> $stanfit
#> Inference for Stan model: impute_school_coverage_process_v6.
#> 2 chains, each with iter=500; warmup=250; thin=1; 
#> post-warmup draws per chain=250, total post-warmup draws=500.
#> 
#>                   mean se_mean   sd     2.5%      25%      50%      75%
#> beta_bs[1]       -1.61    0.01 0.15    -1.93    -1.71    -1.61    -1.52
#> beta_bs[2]       -2.02    0.01 0.12    -2.26    -2.10    -2.02    -1.94
#> beta_bs[3]       -2.05    0.01 0.16    -2.34    -2.15    -2.04    -1.94
#> beta_bs[4]       -3.24    0.01 0.17    -3.57    -3.35    -3.23    -3.13
#> beta_bs[5]       -2.29    0.01 0.20    -2.69    -2.42    -2.29    -2.15
#> sigma_cnty        0.23    0.02 0.19     0.01     0.10     0.18     0.31
#> off_cnty[1]      -0.07    0.01 0.13    -0.36    -0.14    -0.05     0.01
#> off_cnty[2]      -0.14    0.02 0.17    -0.60    -0.24    -0.10    -0.02
#> off_cnty[3]      -0.02    0.01 0.14    -0.34    -0.10    -0.01     0.06
#> sigma_sch         0.57    0.01 0.11     0.40     0.49     0.57     0.64
#> off_sch[1]       -1.07    0.02 0.25    -1.63    -1.22    -1.06    -0.90
#> off_sch[2]       -0.06    0.02 0.15    -0.32    -0.15    -0.07     0.04
#> off_sch[3]       -0.77    0.01 0.18    -1.13    -0.89    -0.77    -0.65
#> off_sch[4]        0.21    0.02 0.17    -0.10     0.09     0.20     0.30
#> off_sch[5]        0.21    0.01 0.15    -0.07     0.12     0.19     0.30
#> off_sch[6]        0.43    0.01 0.16     0.14     0.33     0.42     0.52
#> off_sch[7]        0.27    0.01 0.17    -0.08     0.15     0.26     0.38
#> off_sch[8]       -0.68    0.01 0.21    -1.07    -0.83    -0.68    -0.54
#> off_sch[9]        0.71    0.01 0.14     0.46     0.62     0.70     0.79
#> off_sch[10]      -0.04    0.01 0.14    -0.30    -0.13    -0.05     0.04
#> off_sch[11]       0.22    0.02 0.21    -0.16     0.10     0.22     0.34
#> off_sch[12]      -0.71    0.02 0.23    -1.14    -0.85    -0.72    -0.58
#> off_sch[13]       0.46    0.02 0.19     0.14     0.34     0.45     0.58
#> off_sch[14]      -0.40    0.02 0.21    -0.77    -0.53    -0.41    -0.28
#> off_sch[15]      -0.13    0.02 0.22    -0.51    -0.29    -0.14     0.00
#> off_sch[16]       0.16    0.02 0.19    -0.15     0.03     0.14     0.25
#> off_sch[17]      -1.09    0.02 0.32    -1.77    -1.27    -1.09    -0.88
#> off_sch[18]      -0.86    0.01 0.20    -1.30    -0.99    -0.86    -0.73
#> off_sch[19]       0.37    0.01 0.17     0.01     0.26     0.38     0.47
#> off_sch[20]       0.11    0.01 0.17    -0.22     0.00     0.11     0.20
#> off_sch[21]      -0.36    0.01 0.20    -0.77    -0.49    -0.35    -0.21
#> off_sch[22]      -0.09    0.02 0.23    -0.57    -0.25    -0.06     0.06
#> off_sch[23]       0.06    0.01 0.15    -0.25    -0.02     0.06     0.16
#> off_sch[24]       0.55    0.01 0.17     0.20     0.46     0.55     0.65
#> lambda_raw[1]     1.36    0.05 0.82     0.12     0.73     1.29     1.86
#> lambda_raw[2]     1.16    0.00 0.05     1.09     1.14     1.16     1.18
#> lp__          -1558.23    0.64 5.99 -1571.35 -1562.04 -1557.98 -1553.86
#>                  97.5% n_eff Rhat
#> beta_bs[1]       -1.32   292 1.01
#> beta_bs[2]       -1.82   264 1.01
#> beta_bs[3]       -1.75   250 1.01
#> beta_bs[4]       -2.92   264 1.01
#> beta_bs[5]       -1.89   302 1.00
#> sigma_cnty        0.77    69 1.00
#> off_cnty[1]       0.19    91 1.04
#> off_cnty[2]       0.14    78 1.01
#> off_cnty[3]       0.27   158 1.00
#> sigma_sch         0.79    94 1.02
#> off_sch[1]       -0.67   184 1.01
#> off_sch[2]        0.26    98 1.04
#> off_sch[3]       -0.40   154 1.03
#> off_sch[4]        0.57   110 1.03
#> off_sch[5]        0.52   113 1.03
#> off_sch[6]        0.78   126 1.02
#> off_sch[7]        0.60   151 1.02
#> off_sch[8]       -0.25   223 1.01
#> off_sch[9]        1.03    97 1.03
#> off_sch[10]       0.27    94 1.04
#> off_sch[11]       0.69   108 1.01
#> off_sch[12]      -0.20   161 1.00
#> off_sch[13]       0.91    92 1.01
#> off_sch[14]       0.05   104 1.01
#> off_sch[15]       0.31   152 1.00
#> off_sch[16]       0.59    90 1.01
#> off_sch[17]      -0.50   196 1.00
#> off_sch[18]      -0.47   192 1.01
#> off_sch[19]       0.70   202 1.00
#> off_sch[20]       0.49   207 1.00
#> off_sch[21]       0.01   193 1.00
#> off_sch[22]       0.32   227 1.00
#> off_sch[23]       0.37   194 1.00
#> off_sch[24]       0.89   255 1.00
#> lambda_raw[1]     3.15   240 1.00
#> lambda_raw[2]     1.28   188 1.01
#> lp__          -1547.85    89 1.02
#> 
#> Samples were drawn using NUTS(diag_e) at Thu Jul  9 01:15:50 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
#> 
#> $settings
#> $settings$imugap_opts
#> $settings$imugap_opts$df
#> [1] 5
#> 
#> $settings$imugap_opts$dose_schedule
#> [1] 1 4
#> 
#> $settings$imugap_opts$object
#> S4 class stanmodel 'impute_school_coverage_process_v6' coded as follows:
#> functions {
#>   // a function to convert lower bounds l_1, 1_2, ... 1_n
#>   // to (lower, upper) pairs (l_1, l_2-1), (l_2, l_3-1), ...
#>   array[,] int bounds_to_range(array[] int lowers, int ub) {
#>     int size_bounds = size(lowers);
#>     if (lowers[size_bounds] > ub) {
#>         print("Upper bound, ", ub, " is less than last lower bound, ", lowers[size_bounds]);
#>     }
#>     array[size_bounds] int uppers;
#>     for (i in 1:(size_bounds - 1)) {
#>         uppers[i] = lowers[i+1] - 1;
#>     }
#>     uppers[size_bounds] = ub;
#>     return { lowers, uppers };
#>   }
#>   // create a matrix, each column multiplied by corresponding row entry
#>   matrix element_mult_expand(vector colv, row_vector rowv) {
#>     int nrows = size(colv), ncols = size(rowv);
#>     matrix[nrows, ncols] result;
#>     for (i in 1:nrows) {
#>         result[i,] = rowv * colv[i];
#>     }
#>     return result;
#>   }
#>   // Sequential diff
#>   vector diff(vector obj) {
#>     int sz = size(obj);
#>     return obj[2:] - obj[:(sz-1)];
#>   }
#>   row_vector diff(row_vector obj) {
#>     int sz = size(obj);
#>     return obj[2:] - obj[:(sz-1)];
#>   }
#>   row_vector colsum(matrix obj) {
#>     int ncols = cols(obj);
#>     row_vector[ncols] res;
#>     for (i in 1:ncols) {
#>         res[i] = sum(obj[, i]);
#>     }
#>     return res;
#>   }
#>   vector rowsum(matrix obj) {
#>     int nrows = rows(obj);
#>     vector[nrows] res;
#>     for (i in 1:nrows) {
#>         res[i] = sum(obj[i, ]);
#>     }
#>     return res;
#>   }
#> vector unrolled_dose(int n_yr, int n_doses, matrix dose_sched, vector lambda_raw, real epsilon_p) {
#>   // assert: dose_sched is n_yr x n_doses
#>   // assert: lambda_raw is n_doses x 1 (alt: would be n_doses x n_time)
#>   vector[n_doses] lambda = exp(lambda_raw);
#>   // the unconditional cdfs, given FoV + whatever remains; normalization factors
#>   matrix[n_yr, n_doses] dXcdf, dXpdf, normdXpdf;
#>   vector[n_doses] rem;
#>   for (dose in 1:n_doses) {
#>     dXcdf[, dose] = 1 - exp(-cumulative_sum(dose_sched[, dose] * lambda[dose]));
#>     rem[dose] = 1 - dXcdf[n_yr, dose];
#>     dXpdf[1, dose] = dXcdf[1, dose];
#>     dXpdf[2:,dose] = diff(dXcdf[, dose]);
#>     normdXpdf[, dose] = reverse(cumulative_sum(reverse(dXpdf[, dose]))) + rem[dose];
#>   }
#>   matrix[n_yr, n_doses] conditional_dXpdf = rep_matrix(0, n_yr, n_doses), conditional_dXcdf;
#>   conditional_dXpdf[, 1] = dXpdf[, 1];
#>   conditional_dXcdf[, 1] = dXcdf[, 1];
#>   for (dose in 2:n_doses) {
#>     int prev_dose = dose - 1;
#>     // conditional probability => probability got dose n-1 at some time, then probability got dose n at later times
#>     for (ly in 1:n_yr) {
#>       if (normdXpdf[ly, dose] < epsilon_p) break; // if the remaining weight is negligible, avoid division by zero
#>       conditional_dXpdf[ly:, dose] += conditional_dXpdf[ly, prev_dose] * dXpdf[ly:, dose] / normdXpdf[ly, dose];
#>     }
#>     conditional_dXcdf[, dose] = cumulative_sum(conditional_dXpdf[, dose]);
#>   }
#>   // TODO check unrolling of cdfs - should be dose 1 all years, then dose 2 all years, etc
#>   vector[n_doses * n_yr] unrolled_dose_cdf = to_vector(conditional_dXcdf);
#>   return unrolled_dose_cdf;
#> }
#> }
#> // need meta info:
#> //  which cohorts, which places, which years of life
#> //  => assume each measurement is independent, not progress of some matched individuals
#> data {
#> // #include data/shared_stateonly.stan
#>   // STRUCTURAL DEFINITIONS
#>   int<lower=1> n_yr; // number of years to model for each cohort - should be at least year of oldest observation
#>   int<lower=1> n_cohort; // number of birth year cohorts
#>   int<lower=1> n_sch; // number of schools
#> 
#>   // maps schools to county
#>   int<lower=1> n_cnty;
#>   array[n_cnty] int<lower=1, upper=n_sch> cnty_bounds; // which schools indices start each county
#>   // dose schedules
#>   int<lower=1> n_doses;
#>   matrix<lower=0, upper=1>[n_yr, n_doses] dose_sched;
#>   // DATA DEFINITIONS
#>   int<lower=1> n_obs;
#>   array[n_obs] int<lower=0> y_obs;
#>   array[n_obs] int<lower=0> y_smp;
#>   // have school id ranges for observations & for doses; school id 0 == statewide?
#>   // array[n_obs] int obs_sch_id_bounds;
#>   int<lower=n_obs> n_weights;
#>   array[n_obs] int<lower=1, upper=n_weights> obs_to_weights_bounds; // each entry is the start of the range
#>   array[n_weights] int<lower=1,upper=n_sch + n_cnty + 1> weights_school;
#>   array[n_weights] int<lower=1,upper=n_cohort> weights_cohort;
#>   array[n_weights] int<lower=1,upper=n_yr> weights_life_year;
#>   array[n_weights] int<lower=1,upper=n_doses> weights_dose;
#>   vector<lower=0,upper=1>[n_weights] weights; // contribution of this (school, cohort, year, dose) to an observation
#>   // run mode: 0 = estimation, 1 = prediction
#>   int<lower=0, upper=1> predict_mode;
#>   // TODO: calculate these in stan?
#>   // https://spinkney.github.io/helpful_stan_functions/group__splines.html
#>   // state-level basis spline
#>   int k_bs; // number of bspline basis functions
#>   matrix[n_cohort, k_bs] bs; // basis functions
#> // observations may be right-censored
#> // observation data is assumed ordered uncensored, then right censored
#> // so n_uncensored_obs == n_obs, all observations are uncensored
#> // number of uncensored observations
#> int<lower = 0, upper = n_obs> n_uncensored_obs;
#> }
#> transformed data {
#>    // #include transformed_data/stateonly.stan
#>    // #include transformed_data/fixed_raw_lambda.stan
#>    // #include transformed_data/fixed_logit_phi.stan
#> real epsilon_p = 1e-10;
#>   // convert to lookup spans for convenience
#>   array[2, n_cnty] int cnty_map = bounds_to_range(cnty_bounds, n_sch);
#>   array[2, n_obs] int obs_map = bounds_to_range(obs_to_weights_bounds, n_weights);
#> 
#>   // Equivalent 1:n_cohort, for time trends
#>   vector[n_cohort] cohort_shift_counter = linspaced_vector(n_cohort, 1, n_cohort);
#>   array[n_weights] int<lower=1> phi_lookup;
#>   array[n_weights] int<lower=1> cdf_lookup;
#>   // because integer arrays don't support broadcasting ...
#>   // unroll phi and cdf objects to support vectorization
#>   for (weight_i in 1:n_weights) {
#>     // phi ordered by school then cohort
#>     phi_lookup[weight_i] = weights_cohort[weight_i] + (weights_school[weight_i] - 1) * n_cohort;
#>     // ordered by dose then life year
#>     cdf_lookup[weight_i] = weights_life_year[weight_i] + (weights_dose[weight_i] - 1) * n_yr;
#>   }
#> array[n_obs] int<lower=-1> y_obs_trans;
#> for(i in 1:n_obs) {
#>   y_obs_trans[i] = y_obs[i] - 1;
#> }
#> }
#> parameters {
#>   // bases spline coeficcients
#>   vector[k_bs] beta_bs; // spline betas
#>   // #include parameters/constant_phi.stan
#>   // #include parameters/cnty_sch_linear.stan
#>   // offsets
#>   real<lower=0> sigma_cnty;
#>   row_vector<multiplier=sigma_cnty>[n_cnty] off_cnty;
#>   real<lower=0> sigma_sch;
#>   row_vector<multiplier=sigma_sch>[n_sch] off_sch;
#>   // Vaccination uptake rate
#>   vector[n_doses] lambda_raw;
#> }
#> model {
#>   if (!predict_mode) {
#>     // PRIORS - spline coefficients
#>     beta_bs ~ normal(0, 10);
#>     // PRIOR - lambda; relatively strong prior belief that ~95% coverage achieved in a year
#>     // mean of 3 => 1 - exp(-3*1) == ~ 0.95
#>     lambda_raw ~ normal(log(3), 1);
#>     // Offsets - non-centered parameterization to speed up
#>     sigma_cnty ~ cauchy(0, 1);
#>     off_cnty ~ normal(0, sigma_cnty);
#>     sigma_sch ~ cauchy(0, 2);
#>     off_sch ~ normal(0, sigma_sch);
#>     vector[n_obs] p_obs;
#> vector[n_cohort] logit_phi_st = bs * beta_bs;
#> row_vector[n_sch] shift = off_sch;
#> for (c in 1:n_cnty) {
#>   shift[cnty_map[1,c]:cnty_map[2,c]] += off_cnty[c];
#> }
#> vector[(1+ n_cnty + n_sch) * n_cohort] phi = append_row(append_row(
#>   inv_logit(logit_phi_st),
#>   to_vector(inv_logit(
#>     rep_matrix(logit_phi_st, n_cnty) + rep_matrix(off_cnty, n_cohort)
#>   ))),
#>   to_vector(inv_logit(
#>     rep_matrix(logit_phi_st, n_sch) + rep_matrix(shift, n_cohort)
#>   ))
#> );
#> vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
#> vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
#> for (obs_i in 1:n_obs) {
#>   p_obs[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
#> }
#> if (n_uncensored_obs < n_obs) { // at least some censored observations
#>     // p_s => 1 - p_s = p_f :: probability of at least this many successes =>
#>     //                         probability of less than this many failures
#>     if (n_uncensored_obs > 0) { // at least some uncensored observations
#>         target += binomial_lpmf(y_obs[:n_uncensored_obs] | y_smp[:n_uncensored_obs], p_obs[:n_uncensored_obs]);
#>     }
#>     target += binomial_lcdf(y_obs[(n_uncensored_obs+1):] | y_smp[(n_uncensored_obs+1):], 1 - p_obs[(n_uncensored_obs+1):]);
#> } else { // all uncensored observations
#>     y_obs ~ binomial(y_smp, p_obs); // vectorized
#> }
#>   }
#> }
#> generated quantities {
#>   vector[predict_mode ? n_obs : 0] p_obs;
#>   if (predict_mode) {
#> vector[n_cohort] logit_phi_st = bs * beta_bs;
#> row_vector[n_sch] shift = off_sch;
#> for (c in 1:n_cnty) {
#>   shift[cnty_map[1,c]:cnty_map[2,c]] += off_cnty[c];
#> }
#> vector[(1+ n_cnty + n_sch) * n_cohort] phi = append_row(append_row(
#>   inv_logit(logit_phi_st),
#>   to_vector(inv_logit(
#>     rep_matrix(logit_phi_st, n_cnty) + rep_matrix(off_cnty, n_cohort)
#>   ))),
#>   to_vector(inv_logit(
#>     rep_matrix(logit_phi_st, n_sch) + rep_matrix(shift, n_cohort)
#>   ))
#> );
#> vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);
#> vector[n_weights] weighted = (1 - phi[phi_lookup]) .* unrolled_dose_probs[cdf_lookup] .* weights;
#> for (obs_i in 1:n_obs) {
#>   p_obs[obs_i] = sum(weighted[obs_map[1,obs_i]:obs_map[2,obs_i]]);
#> }
#>   }
#> }
#> // This model represents vaccination as a discrete step, fixed hazard process
#> // therefore X(t) => X(t + deltaT) = X(t)*exp(-hazard*deltaT)
#> // which means the X-out flow == X(t)*(1-exp(-hazard*deltaT))
#> //
#> // for an arbitrary number of hazards & deltaTs, we simply sum:
#> // net outflow: X(t)*(1-exp(-sum(hazard*deltaT)))
#> //
#> // This model represents an overall hazard, shared across birth cohorts and
#> // vaccine doses. Additionally, birth cohorts have a propensity to vaccinate,
#> // which is fixed at birth and then applies throughout life.
#> //
#> // deltaT captures both time passage + eligibility. deltaT in the fitting is
#> // always == 1, but note that this is unitless - the data define what time means
#> // e.g. if birth cohorts are based on years, then deltaT is years, and a weight
#> // of 1 means eligible for a whole year, .25 eligible for a quarter, etc
#> //
#> // the hazard is constant for any given discrete time step, but can vary between
#> // time steps. the hazard time is denoted in absolute model time. however,
#> // cohort time is relative. so:
#> // hazard time == 1: one deltaT has passed, absolute time is 1
#> // cohort 1, time 1: one deltaT has passed => cohort 1 is 1 deltaT old,
#> //   absolute time is also 1 (cohort 1 is the first cohort)
#> // cohort 2, time 2: two deltaT have passed => cohort 2 is 2 deltaT old, but
#> //   cohort 1 is 3 deltaT old, and absolute time is 3
#> // generally:
#> // cohort n, time t: experienced t deltaTs; those deltaT corresponded to absolute
#> //   times n, n+1, ..., n+t
#> //
#> // we assume each cohort has the same doses schedule, so the same deltaT weights
#> //
#> // so for cohort n, fraction of (non-phi/vaccinators) with 1+ doses at age 1:t:
#> //   d1cdf = 1 - exp(-cumsum(weight[1:t] .* lambda[n:t]))
#> //
#> // and the incidence of new dose 1s is:
#> //
#> //   c(d1cdf[1], diff(d1cdf))
#> //
#> // for subsequent doses, the hazard model is the same, but now is conditional on
#> // having received the first dose; thus the incremental hazard is still
#> //   d2cdf = 1 - exp(-cumsum(weight2[1:t] .* lambda[n:t]))
#> //   (note the different eligibility mask, weight2; assert weight2 strictly less than weight1)
#> // but only the fraction having dose 1 experience it.
#> // if we think about this in terms of newly-dose-1-in-deltaT-x:
#> //   we can reframe in these terms:
#> //   define d2pdf = c(d2cdf[1], diff(d2cdf)); pgtt = 1-d2cdf[t] (probably event has not occured by t)
#> //   probability of dose 2 in year (x+1):t => d2pdf[(x+1):t]/sum(d2pdf[x+1:t] + pggt)
#> //   we can then assign each incremental incidence of dose 1 in deltaT x to getting
#> //   second dose in deltaT x+1:t (or not getting it by t)
#> //
#> // this can be built-up as a matrix once per cohort
#> //  1. calculate d2cdf, d2pdf, pgtt as above, for t == max cohort "age"
#> //  2. calculate remd2pdf = rev(cumsum(rev(d2pdf))) + pgtt
#> //  3. d2contrib =
#> //    upper_triangle_1s * colwise element multiplication of d2pdf / rowwise remd2pdf * rowwise d2incidence => column sums => proportion in d2 
#> 
#> 
#> $settings$stan_opts
#> $settings$stan_opts$iter
#> [1] 500
#> 
#> $settings$stan_opts$chains
#> [1] 2
#> 
#> $settings$stan_opts$backend
#> [1] "rstan"
#> 
#> 
#> 
#> $data
#> $data$n_uncensored_obs
#> [1] 638
#> 
#> $data$n_yr
#> [1] 18
#> 
#> $data$n_cohort
#> [1] 31
#> 
#> $data$n_sch
#> [1] 24
#> 
#> $data$n_doses
#> [1] 2
#> 
#> $data$dose_sched
#>       [,1] [,2]
#>  [1,]    0    0
#>  [2,]    1    0
#>  [3,]    1    0
#>  [4,]    1    0
#>  [5,]    1    1
#>  [6,]    1    1
#>  [7,]    1    1
#>  [8,]    1    1
#>  [9,]    1    1
#> [10,]    1    1
#> [11,]    1    1
#> [12,]    1    1
#> [13,]    1    1
#> [14,]    1    1
#> [15,]    1    1
#> [16,]    1    1
#> [17,]    1    1
#> [18,]    1    1
#> 
#> $data$k_bs
#> [1] 5
#> 
#> $data$bs
#>                  1            2          3            4            5
#>  [1,] 1.0000000000 0.000000e+00 0.00000000 0.000000e+00 0.0000000000
#>  [2,] 0.8130370370 1.805185e-01 0.00637037 7.407407e-05 0.0000000000
#>  [3,] 0.6509629630 3.241481e-01 0.02429630 5.925926e-04 0.0000000000
#>  [4,] 0.5120000000 4.340000e-01 0.05200000 2.000000e-03 0.0000000000
#>  [5,] 0.3943703704 5.131852e-01 0.08770370 4.740741e-03 0.0000000000
#>  [6,] 0.2962962963 5.648148e-01 0.12962963 9.259259e-03 0.0000000000
#>  [7,] 0.2160000000 5.920000e-01 0.17600000 1.600000e-02 0.0000000000
#>  [8,] 0.1517037037 5.978519e-01 0.22503704 2.540741e-02 0.0000000000
#>  [9,] 0.1016296296 5.854815e-01 0.27496296 3.792593e-02 0.0000000000
#> [10,] 0.0640000000 5.580000e-01 0.32400000 5.400000e-02 0.0000000000
#> [11,] 0.0370370370 5.185185e-01 0.37037037 7.407407e-02 0.0000000000
#> [12,] 0.0189629630 4.701481e-01 0.41229630 9.859259e-02 0.0000000000
#> [13,] 0.0080000000 4.160000e-01 0.44800000 1.280000e-01 0.0000000000
#> [14,] 0.0023703704 3.591852e-01 0.47570370 1.627407e-01 0.0000000000
#> [15,] 0.0002962963 3.028148e-01 0.49362963 2.032593e-01 0.0000000000
#> [16,] 0.0000000000 2.500000e-01 0.50000000 2.500000e-01 0.0000000000
#> [17,] 0.0000000000 2.032593e-01 0.49362963 3.028148e-01 0.0002962963
#> [18,] 0.0000000000 1.627407e-01 0.47570370 3.591852e-01 0.0023703704
#> [19,] 0.0000000000 1.280000e-01 0.44800000 4.160000e-01 0.0080000000
#> [20,] 0.0000000000 9.859259e-02 0.41229630 4.701481e-01 0.0189629630
#> [21,] 0.0000000000 7.407407e-02 0.37037037 5.185185e-01 0.0370370370
#> [22,] 0.0000000000 5.400000e-02 0.32400000 5.580000e-01 0.0640000000
#> [23,] 0.0000000000 3.792593e-02 0.27496296 5.854815e-01 0.1016296296
#> [24,] 0.0000000000 2.540741e-02 0.22503704 5.978519e-01 0.1517037037
#> [25,] 0.0000000000 1.600000e-02 0.17600000 5.920000e-01 0.2160000000
#> [26,] 0.0000000000 9.259259e-03 0.12962963 5.648148e-01 0.2962962963
#> [27,] 0.0000000000 4.740741e-03 0.08770370 5.131852e-01 0.3943703704
#> [28,] 0.0000000000 2.000000e-03 0.05200000 4.340000e-01 0.5120000000
#> [29,] 0.0000000000 5.925926e-04 0.02429630 3.241481e-01 0.6509629630
#> [30,] 0.0000000000 7.407407e-05 0.00637037 1.805185e-01 0.8130370370
#> [31,] 0.0000000000 0.000000e+00 0.00000000 0.000000e+00 1.0000000000
#> attr(,"degree")
#> [1] 3
#> attr(,"knots")
#> [1] 16
#> attr(,"Boundary.knots")
#> [1]  1 31
#> attr(,"intercept")
#> [1] TRUE
#> attr(,"class")
#> [1] "bs"     "basis"  "matrix"
#> 
#> $data$n_obs
#> [1] 698
#> 
#> $data$y_obs
#>   [1]   42   41   46   41   44   47   46   44   43   44   38   42   47   41   48
#>  [16]   48   40   46   47   50   46   48   42   39   39   28   30   26   29   30
#>  [31]   22   22   19   17   18   18   23   21   24   22   25   27   30   32   28
#>  [46]   29   25   26   21   27   60   61   62   55   56   57   59   63   67   65
#>  [61]   58   67   66   71   71   71   69   71   67   64   68   63   72   69   67
#>  [76]   43   41   49   47   45   49   45   52   50   54   56   55   59   58   57
#>  [91]   61   57   56   59   51   57   53   58   63   60  107  107  114  109  109
#> [106]  106  106  100   91  102  105  105  111  113  115  112  112  112  113  116
#> [121]  119  119  130  128  131  100  103  106  106  112  107  119  117  111  110
#> [136]  113  105  105  105  102  108  109  108  109  107  108  108   94   92   97
#> [151]   82   71   73   64   81   84   80   81   74   78   80   73   71   76   89
#> [166]   90   94   91   93  100  101   98  103  101   97   52   54   48   45   48
#> [181]   41   43   46   48   48   47   42   42   45   41   47   43   45   51   54
#> [196]   58   57   62   62   59  213  227  208  230  223  214  234  234  231  232
#> [211]  244  236  230  240  246  260  262  253  248  254  263  251  254  251  260
#> [226]  186  190  190  198  199  208  198  189  202  201  209  196  206  209  208
#> [241]  209  209  213  206  213  208  210  216  214  209   64   68   70   67   76
#> [256]   69   72   71   66   69   63   72   72   76   71   73   72   69   70   72
#> [271]   68   67   66   62   65   64   68   64   65   66   72   72   66   71   70
#> [286]   71   72   77   73   73   76   78   75   82   85   83   81   76   74   75
#> [301]   35   37   37   39   31   33   37   42   39   34   35   33   33   31   32
#> [316]   27   28   30   30   26   24   25   24   23   20   39   45   46   49   51
#> [331]   46   43   38   49   44   44   47   38   42   42   36   37   36   37   36
#> [346]   39   35   31   25   23   71   84   82   84   87   91   85   95   96   99
#> [361]   99   94  105  102   97  100   97  100  103  110  112  115  114  116  122
#> [376]   97   87   94   87   85   88   93   93   92   90   93   89   99   97   95
#> [391]   96  104  105  100  104  101  100   97   97   98   33   33   37   42   39
#> [406]   35   36   34   30   32   35   36   30   29   30   32   34   30   28   32
#> [421]   22   31   29   28   29   16   17   21   16   24   20   25   30   24   18
#> [436]   19   15   17   15   15   22   23   23   21   25   25   25   21   18   14
#> [451]   43   38   46   42   46   50   41   41   42   40   40   38   41   43   40
#> [466]   39   38   42   39   45   42   41   39   39   42   82   79   89   99   97
#> [481]  103   95  104   97  103  106  104  114  112  107  111  117  117  116  120
#> [496]  124  118  119  122  119   46   47   39   41   37   43   42   43   42   52
#> [511]   53   48   54   52   47   52   54   53   50   44   44   42   38   39   37
#> [526]   38   31   30   30   31   30   32   35   44   41   45   47   37   39   45
#> [541]   47   46   49   50   40   35   38   35   35   39   42   43   42   47   43
#> [556]   44   47   55   45   50   47   50   52   60   54   55   53   50   56   53
#> [571]   61   67   59   56   60   93   91   97   95   89   87   84   84   84   89
#> [586]   93   90   90   91   91   91   89   90   87   89   88   91   89   90   94
#> [601] 1516 1517 1550 1559 1567 1571 1581 1598 1560 1602 1628 1588 1629 1673 1669
#> [616] 1697 1685 1694 1688 1744 1743 1741 1709 1669 1711  235  194  220  223  246
#> [631]  256  240  251  254  252  230  272  210  314  303  238  226  325  231  225
#> [646]  235  193  210  242  269  279  358  295  289  257  296  220  280  238  317
#> [661]  322  355  267  238  245  214  263  311  320  309  239  231  329  233  226
#> [676]  239  196  214  243  274  281  363  301  289  260  298  221  284  241  321
#> [691]  325  362  268  242  247  220  267  315
#> 
#> $data$y_smp
#>   [1]   52   51   50   48   53   54   54   51   52   52   48   50   54   49   54
#>  [16]   53   51   53   54   54   55   51   48   48   43   34   38   36   34   33
#>  [31]   31   27   23   23   22   23   27   24   27   25   28   29   33   34   31
#>  [46]   32   27   28   26   31   63   66   67   63   61   60   64   68   71   69
#>  [61]   66   70   71   73   76   74   75   75   70   70   75   70   74   75   73
#>  [76]   51   55   60   59   58   60   62   64   62   66   65   66   67   72   69
#>  [91]   67   70   65   67   64   63   60   63   68   69  129  125  128  124  125
#> [106]  121  120  116  112  115  120  124  126  127  128  126  128  128  129  133
#> [121]  135  134  138  139  139  112  114  117  120  124  125  124  127  123  123
#> [136]  118  116  115  113  112  115  115  116  116  112  114  110  105  104  104
#> [151]   94   91   88   89   94   97   95   95   91   88   92   88   88   92   97
#> [166]  100  101  104  107  111  115  112  113  113  111   59   60   57   53   51
#> [181]   48   46   50   51   54   52   49   46   48   44   49   48   48   52   56
#> [196]   59   60   64   65   66  296  296  292  292  297  294  292  296  300  301
#> [211]  298  294  297  301  300  304  304  300  295  297  296  300  298  298  299
#> [226]  229  229  232  231  232  232  230  226  228  225  230  231  228  231  234
#> [241]  239  234  232  233  237  235  231  234  233  237   83   84   83   86   90
#> [256]   90   86   87   86   82   80   84   87   89   85   83   83   83   81   79
#> [271]   76   76   75   71   71   71   74   74   74   72   77   77   74   77   77
#> [286]   78   78   81   79   79   81   84   85   89   89   88   85   81   83   80
#> [301]   40   44   42   43   40   40   43   46   44   40   41   40   41   37   35
#> [316]   30   34   35   35   32   28   29   28   24   26   44   49   49   53   52
#> [331]   49   45   48   50   46   46   47   42   47   43   39   40   37   39   40
#> [346]   41   36   32   28   25   93   97  100   95  100  105  109  112  115  117
#> [361]  115  112  116  116  117  113  112  112  115  118  121  126  128  127  130
#> [376]  110  105  102  101  100  101  106  103   99  100   99  102  106  104  107
#> [391]  106  107  110  108  112  108  110  106  108  107   39   38   42   46   44
#> [406]   42   42   40   36   37   38   38   34   30   32   35   37   34   32   32
#> [421]   30   33   33   32   33   22   19   23   22   25   24   28   31   27   22
#> [436]   21   16   19   20   18   22   26   27   24   28   28   29   24   19   16
#> [451]   54   53   56   56   59   57   53   50   48   50   46   46   45   49   49
#> [466]   49   47   49   45   47   46   45   46   46   49  102  103  108  112  117
#> [481]  119  119  116  116  118  121  123  127  123  123  128  127  128  132  134
#> [496]  139  136  135  136  134   50   51   48   47   48   49   45   49   54   54
#> [511]   55   57   60   61   57   54   59   57   54   49   47   43   42   39   39
#> [526]   48   45   45   40   40   41   43   47   49   54   56   53   49   52   52
#> [541]   51   54   55   52   47   42   41   40   42   44   53   53   54   51   55
#> [556]   51   56   59   58   55   57   59   61   62   63   63   62   58   61   62
#> [571]   66   71   67   67   68  102  102  102  101  101   98   96   96   95   99
#> [586]  101   97  101   98   95   96   96   95   92   92   95   94   96   96   99
#> [601] 1835 1832 1859 1845 1858 1863 1858 1867 1843 1854 1857 1840 1872 1902 1906
#> [616] 1896 1885 1891 1882 1929 1934 1921 1887 1875 1903  269  237  248  256  273
#> [631]  289  270  283  281  274  251  296  230  415  400  311  289  436  290  296
#> [646]  292  251  259  303  329  344  432  363  355  307  367  267  333  279  386
#> [661]  396  433  318  276  297  276  328  374  415  400  311  289  436  290  296
#> [676]  292  251  259  303  329  344  432  363  355  307  367  267  333  279  386
#> [691]  396  433  318  276  297  276  328  374
#> 
#> $data$n_weights
#> [1] 750
#> 
#> $data$obs_to_weights_bounds
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
#> [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
#> [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
#> [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
#> [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
#> [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
#> [199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
#> [217] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234
#> [235] 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
#> [253] 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270
#> [271] 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288
#> [289] 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306
#> [307] 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324
#> [325] 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342
#> [343] 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360
#> [361] 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378
#> [379] 379 380 381 382 383 384 385 386 387 388 389 390 391 392 393 394 395 396
#> [397] 397 398 399 400 401 402 403 404 405 406 407 408 409 410 411 412 413 414
#> [415] 415 416 417 418 419 420 421 422 423 424 425 426 427 428 429 430 431 432
#> [433] 433 434 435 436 437 438 439 440 441 442 443 444 445 446 447 448 449 450
#> [451] 451 452 453 454 455 456 457 458 459 460 461 462 463 464 465 466 467 468
#> [469] 469 470 471 472 473 474 475 476 477 478 479 480 481 482 483 484 485 486
#> [487] 487 488 489 490 491 492 493 494 495 496 497 498 499 500 501 502 503 504
#> [505] 505 506 507 508 509 510 511 512 513 514 515 516 517 518 519 520 521 522
#> [523] 523 524 525 526 527 528 529 530 531 532 533 534 535 536 537 538 539 540
#> [541] 541 542 543 544 545 546 547 548 549 550 551 552 553 554 555 556 557 558
#> [559] 559 560 561 562 563 564 565 566 567 568 569 570 571 572 573 574 575 576
#> [577] 577 578 579 580 581 582 583 584 585 586 587 588 589 590 591 592 593 594
#> [595] 595 596 597 598 599 600 601 602 603 604 605 606 607 608 609 610 611 612
#> [613] 613 614 615 616 617 618 619 620 621 622 623 624 625 626 631 636 641 646
#> [631] 651 656 661 666 671 676 681 686 691 692 693 694 695 696 697 698 699 700
#> [649] 701 702 703 704 705 706 707 708 709 710 711 712 713 714 715 716 717 718
#> [667] 719 720 721 722 723 724 725 726 727 728 729 730 731 732 733 734 735 736
#> [685] 737 738 739 740 741 742 743 744 745 746 747 748 749 750
#> 
#> $data$weights_school
#>   [1]  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8
#>  [26] 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
#>  [51]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#>  [76] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#> [101]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
#> [126]  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7
#> [151]  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9  9
#> [176] 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
#> [201] 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
#> [226] 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
#> [251] 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17 17
#> [276] 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16
#> [301] 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15
#> [326] 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21 21
#> [351] 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
#> [376] 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18
#> [401] 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19 19
#> [426] 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26
#> [451] 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23
#> [476] 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27 27
#> [501] 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
#> [526] 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28
#> [551] 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
#> [576] 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22
#> [601]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
#> [626]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
#> [651]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
#> [676]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
#> [701]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
#> [726]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
#> 
#> $data$weights_cohort
#>   [1]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>  [26]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>  [51]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#>  [76]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [101]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [126]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [151]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [176]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [201]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [226]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [251]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [276]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [301]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [326]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [351]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [376]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [401]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [426]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [451]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [476]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [501]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [526]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [551]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [576]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [601]  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
#> [626]  3  4  5  6  7  4  5  6  7  8  5  6  7  8  9  6  7  8  9 10  7  8  9 10 11
#> [651]  8  9 10 11 12  9 10 11 12 13 10 11 12 13 14 11 12 13 14 15 12 13 14 15 16
#> [676] 13 14 15 16 17 14 15 16 17 18 15 16 17 18 19  2  3  4  5  6  7  8  9 10 11
#> [701] 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31  1  2  3  4  5
#> [726]  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
#> 
#> $data$weights_life_year
#>   [1]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#>  [26]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#>  [51]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#>  [76]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [101]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [126]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [151]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [176]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [201]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [226]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [251]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [276]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [301]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [326]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [351]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [376]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [401]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [426]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [451]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [476]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [501]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [526]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [551]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [576]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [601]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5
#> [626] 18 17 16 15 14 18 17 16 15 14 18 17 16 15 14 18 17 16 15 14 18 17 16 15 14
#> [651] 18 17 16 15 14 18 17 16 15 14 18 17 16 15 14 18 17 16 15 14 18 17 16 15 14
#> [676] 18 17 16 15 14 18 17 16 15 14 18 17 16 15 14  2  2  2  2  2  2  2  2  2  2
#> [701]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3
#> [726]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3
#> 
#> $data$weights_dose
#>   [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [38] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#>  [75] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [112] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [149] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [186] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [223] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [260] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [297] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [334] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [371] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [408] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [445] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [482] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [519] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [556] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [593] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [630] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
#> [667] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1
#> [704] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> [741] 1 1 1 1 1 1 1 1 1 1
#> 
#> $data$weights
#>   [1] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#>  [19] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#>  [37] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#>  [55] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#>  [73] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#>  [91] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [109] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [127] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [145] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [163] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [181] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [199] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [217] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [235] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [253] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [271] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [289] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [307] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [325] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [343] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [361] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [379] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [397] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [415] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [433] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [451] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [469] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [487] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [505] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [523] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [541] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [559] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [577] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [595] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [613] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.2 0.2 0.2 0.2 0.2
#> [631] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
#> [649] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
#> [667] 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
#> [685] 0.2 0.2 0.2 0.2 0.2 0.2 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [703] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [721] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> [739] 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
#> 
#> $data$n_cnty
#> [1] 3
#> 
#> $data$cnty_bounds
#> [1]  1 11 18
#> 
#> $data$predict_mode
#> [1] 0
#> 
#> 
#> $locations
#> Key: <layer, parent_id, loc_id>
#>                        loc_id parent_id layer loc_c_id loc_cp_id layer_bound
#>                        <char>    <char> <int>    <int>     <int>       <int>
#>  1:                     State      <NA>     1        1        NA           1
#>  2:                   Scruggs     State     2        2         1           1
#>  3:                    Simone     State     2        3         1           1
#>  4:                    Watson     State     2        4         1           1
#>  5:         Blue Heron School   Scruggs     3        5         2           1
#>  6:  Bluebird Learning Center   Scruggs     3        6         2           1
#>  7:           Catbird Academy   Scruggs     3        7         2           1
#>  8:      Chickadee Elementary   Scruggs     3        8         2           1
#>  9:          Finch Elementary   Scruggs     3        9         2           1
#> 10:     Flycatcher Elementary   Scruggs     3       10         2           1
#> 11:          Nuthatch Academy   Scruggs     3       11         2           1
#> 12:            Sparrow School   Scruggs     3       12         2           1
#> 13: Towhee Children's Academy   Scruggs     3       13         2           1
#> 14:        Warbler Elementary   Scruggs     3       14         2           1
#> 15:            Bunting School    Simone     3       15         3          11
#> 16:          Cardinal Academy    Simone     3       16         3          11
#> 17:          Egret Elementary    Simone     3       17         3          11
#> 18:  Grosbeak Learning Center    Simone     3       18         3          11
#> 19:          Junco Elementary    Simone     3       19         3          11
#> 20:      Oriole Youth Academy    Simone     3       20         3          11
#> 21:           Tanager Academy    Simone     3       21         3          11
#> 22:      Cormorant Elementary    Watson     3       22         4          18
#> 23:      Goldfinch Elementary    Watson     3       23         4          18
#> 24:        Kingfisher Academy    Watson     3       24         4          18
#> 25:   Kinglet Learning Center    Watson     3       25         4          18
#> 26:         Meadowlark School    Watson     3       26         4          18
#> 27:       Mockingbird Academy    Watson     3       27         4          18
#> 28:              Vireo School    Watson     3       28         4          18
#>                        loc_id parent_id layer loc_c_id loc_cp_id layer_bound
#>                        <char>    <char> <int>    <int>     <int>       <int>
#> 
#> attr(,"class")
#> [1] "imugap_fit"
# }
```
