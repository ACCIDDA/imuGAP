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
#> Chain 1: Gradient evaluation took 0.000215 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 2.15 seconds.
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
#> Chain 1:  Elapsed Time: 10.985 seconds (Warm-up)
#> Chain 1:                4.894 seconds (Sampling)
#> Chain 1:                15.879 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'impute_school_coverage_process_v6' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 0.000194 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 1.94 seconds.
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
#> Chain 2:  Elapsed Time: 10.743 seconds (Warm-up)
#> Chain 2:                5.873 seconds (Sampling)
#> Chain 2:                16.616 seconds (Total)
#> Chain 2: 
#> Warning: There were 6 divergent transitions after warmup. See
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
#> beta_bs[1]       -1.82    0.01 0.15    -2.11    -1.92    -1.82    -1.70
#> beta_bs[2]       -1.73    0.01 0.12    -1.95    -1.81    -1.73    -1.65
#> beta_bs[3]       -2.37    0.01 0.16    -2.65    -2.48    -2.36    -2.26
#> beta_bs[4]       -3.07    0.01 0.18    -3.42    -3.20    -3.07    -2.95
#> beta_bs[5]       -2.81    0.01 0.24    -3.30    -2.98    -2.80    -2.64
#> sigma_cnty        0.63    0.04 0.40     0.09     0.33     0.57     0.85
#> off_cnty[1]      -0.30    0.02 0.26    -0.81    -0.46    -0.29    -0.12
#> off_cnty[2]      -0.52    0.04 0.33    -1.22    -0.74    -0.52    -0.29
#> off_cnty[3]      -0.31    0.03 0.33    -1.03    -0.51    -0.29    -0.06
#> sigma_sch         0.87    0.02 0.18     0.56     0.74     0.84     0.98
#> off_sch[1]       -1.14    0.03 0.40    -2.03    -1.37    -1.13    -0.86
#> off_sch[2]       -0.16    0.03 0.28    -0.71    -0.34    -0.17     0.01
#> off_sch[3]       -0.96    0.03 0.39    -1.86    -1.19    -0.94    -0.70
#> off_sch[4]        0.32    0.02 0.34    -0.40     0.11     0.32     0.54
#> off_sch[5]        0.26    0.02 0.28    -0.27     0.08     0.25     0.45
#> off_sch[6]        0.38    0.03 0.27    -0.10     0.19     0.38     0.54
#> off_sch[7]        0.38    0.02 0.27    -0.09     0.19     0.37     0.54
#> off_sch[8]       -1.18    0.04 0.49    -2.25    -1.42    -1.16    -0.86
#> off_sch[9]        0.83    0.02 0.26     0.36     0.66     0.83     0.99
#> off_sch[10]      -0.07    0.02 0.28    -0.59    -0.25    -0.07     0.12
#> off_sch[11]       0.42    0.04 0.34    -0.21     0.17     0.43     0.65
#> off_sch[12]      -1.02    0.04 0.50    -2.11    -1.32    -0.98    -0.70
#> off_sch[13]       0.61    0.04 0.35    -0.03     0.38     0.62     0.83
#> off_sch[14]      -0.42    0.04 0.35    -1.05    -0.65    -0.40    -0.19
#> off_sch[15]       0.02    0.04 0.33    -0.58    -0.22     0.01     0.23
#> off_sch[16]       0.54    0.04 0.38    -0.16     0.29     0.54     0.80
#> off_sch[17]      -1.86    0.04 0.60    -3.31    -2.21    -1.80    -1.43
#> off_sch[18]      -1.26    0.03 0.41    -2.07    -1.53    -1.27    -0.99
#> off_sch[19]       0.37    0.03 0.33    -0.19     0.13     0.33     0.58
#> off_sch[20]       0.02    0.03 0.34    -0.55    -0.21    -0.02     0.22
#> off_sch[21]      -1.00    0.04 0.50    -2.12    -1.29    -0.97    -0.69
#> off_sch[22]      -0.09    0.03 0.42    -0.94    -0.37    -0.10     0.18
#> off_sch[23]       0.12    0.03 0.35    -0.50    -0.12     0.08     0.35
#> off_sch[24]       0.70    0.03 0.34     0.12     0.44     0.68     0.92
#> lambda_raw[1]     1.37    0.05 0.79     0.17     0.73     1.31     1.90
#> lambda_raw[2]     1.04    0.00 0.03     1.00     1.02     1.04     1.06
#> lp__          -1586.12    0.54 5.73 -1597.40 -1589.80 -1585.80 -1581.87
#>                  97.5% n_eff Rhat
#> beta_bs[1]       -1.55   268 1.00
#> beta_bs[2]       -1.51   240 1.00
#> beta_bs[3]       -2.07   257 1.00
#> beta_bs[4]       -2.75   250 1.00
#> beta_bs[5]       -2.37   368 1.00
#> sigma_cnty        1.62    91 1.01
#> off_cnty[1]       0.17   112 1.00
#> off_cnty[2]       0.10    81 1.01
#> off_cnty[3]       0.25   107 1.01
#> sigma_sch         1.28   145 1.03
#> off_sch[1]       -0.42   204 1.00
#> off_sch[2]        0.45   123 1.00
#> off_sch[3]       -0.24   170 1.01
#> off_sch[4]        0.98   192 1.00
#> off_sch[5]        0.86   136 1.00
#> off_sch[6]        0.91   109 1.00
#> off_sch[7]        0.90   128 1.00
#> off_sch[8]       -0.41   153 1.01
#> off_sch[9]        1.37   115 1.00
#> off_sch[10]       0.53   126 1.00
#> off_sch[11]       1.10    89 1.01
#> off_sch[12]      -0.14   195 1.00
#> off_sch[13]       1.32    85 1.02
#> off_sch[14]       0.25    87 1.01
#> off_sch[15]       0.68    87 1.01
#> off_sch[16]       1.27    91 1.01
#> off_sch[17]      -0.90   211 1.00
#> off_sch[18]      -0.46   160 1.01
#> off_sch[19]       1.10   109 1.01
#> off_sch[20]       0.77   125 1.01
#> off_sch[21]      -0.01   190 1.00
#> off_sch[22]       0.77   186 1.01
#> off_sch[23]       0.84   124 1.01
#> off_sch[24]       1.47   112 1.01
#> lambda_raw[1]     3.25   225 1.00
#> lambda_raw[2]     1.11   214 1.00
#> lp__          -1576.30   112 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Tue Jun 16 20:11:48 2026.
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
#>   int<lower=0> y_obs[n_obs];
#>   int<lower=0> y_smp[n_obs];
#>   // have school id ranges for observations & for doses; school id 0 == statewide?
#>   // int obs_sch_id_bounds[n_obs];
#>   int<lower=n_obs> n_weights;
#>   array[n_obs] int<lower=1, upper=n_weights> obs_to_weights_bounds; // each entry is the start of the range
#>   int<lower=1,upper=n_sch + n_cnty + 1> weights_school[n_weights];
#>   int<lower=1,upper=n_cohort> weights_cohort[n_weights];
#>   int<lower=1,upper=n_yr> weights_life_year[n_weights];
#>   int<lower=1,upper=n_doses> weights_dose[n_weights];
#>   vector<lower=0,upper=1>[n_weights] weights; // contribution of this (school, cohort, year, dose) to an observation
#>   // run mode: 0 = estimation, 1 = prediction
#>   int<lower=0, upper=1> predict_mode;
#>   // TODO: calculate these in stan?
#>   // https://spinkney.github.io/helpful_stan_functions/group__splines.html
#>   // state-level basis spline
#>   int k_bs; // number of bspline basis functions
#>   matrix[n_cohort, k_bs] bs; // basis functions
#> # observations may be right-censored
#> # observation data is assumed ordered uncensored, then right censored
#> # so n_uncensored_obs == n_obs, all observations are uncensored
#> # number of uncensored observations
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
#>   int<lower=1> phi_lookup[n_weights];
#>   int<lower=1> cdf_lookup[n_weights];
#>   // because integer arrays don't support broadcasting ...
#>   // unroll phi and cdf objects to support vectorization
#>   for (weight_i in 1:n_weights) {
#>     // phi ordered by school then cohort
#>     phi_lookup[weight_i] = weights_cohort[weight_i] + (weights_school[weight_i] - 1) * n_cohort;
#>     // ordered by dose then life year
#>     cdf_lookup[weight_i] = weights_life_year[weight_i] + (weights_dose[weight_i] - 1) * n_yr;
#>   }
#> int<lower=-1> y_obs_trans[n_obs];
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
#> if (n_uncensored_obs < n_obs) { # at least some censored observations
#>     # p_s => 1 - p_s = p_f :: probability of at least this many successes =>
#>     #                         probability of less than this many failures
#>     if (n_uncensored_obs > 0) { # at least some uncensored observations
#>         target += binomial_lpmf(y_obs[:n_uncensored_obs] | y_smp[:n_uncensored_obs], p_obs[:n_uncensored_obs]);
#>     }
#>     target += binomial_lcdf(y_obs[(n_uncensored_obs+1):] | y_smp[(n_uncensored_obs+1):], 1 - p_obs[(n_uncensored_obs+1):]);
#> } else { # all uncensored observations
#>     y_obs ~ binomial(y_smp, p_obs); # vectorized
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
#> $settings$stan_opts$chains
#> [1] 2
#> 
#> $settings$stan_opts$iter
#> [1] 500
#> 
#> $settings$stan_opts$data
#> $settings$stan_opts$data$n_uncensored_obs
#> [1] 638
#> 
#> $settings$stan_opts$data$n_yr
#> [1] 18
#> 
#> $settings$stan_opts$data$n_cohort
#> [1] 31
#> 
#> $settings$stan_opts$data$n_sch
#> [1] 24
#> 
#> $settings$stan_opts$data$n_doses
#> [1] 2
#> 
#> $settings$stan_opts$data$dose_sched
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
#> $settings$stan_opts$data$k_bs
#> [1] 5
#> 
#> $settings$stan_opts$data$bs
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
#> $settings$stan_opts$data$n_obs
#> [1] 698
#> 
#> $settings$stan_opts$data$y_obs
#>   [1]   16   14   14   10    8    7    6    9    5    4    4    4    4    4    6
#>  [16]    6    8   12   10   14   11   10   13   16   19   43   44   42   42   35
#>  [31]   42   41   43   51   48   44   43   44   46   54   56   54   57   57   54
#>  [46]   57   54   52   54   48   59   63   58   67   64   62   58   56   60   65
#>  [61]   65   65   64   67   62   64   67   64   61   63   62   57   65   60   60
#>  [76]   69   79   76   82   88   83   87   84   91   87   97   93  101  103   98
#>  [91]   96   97  103  102  106  112  105  111  112  108   71   73   73   85   83
#> [106]   87   82   82   79   77   79   80   75   83   85   88   86   90   91   92
#> [121]   89   93   96  101  108   58   59   58   45   44   48   52   52   49   47
#> [136]   52   59   54   54   55   54   55   55   54   52   57   57   53   52   50
#> [151]   50   51   48   53   51   48   46   52   52   55   54   50   45   47   49
#> [166]   49   50   53   53   62   48   53   49   47   47   46   50   49   54   54
#> [181]   50   54   57   58   57   61   64   58   57   56   61   63   63   62   64
#> [196]   59   59   65   65   55  283  288  278  279  280  279  290  290  308  309
#> [211]  290  325  297  318  305  321  318  320  338  337  340  319  330  333  332
#> [226]   68   61   65   72   68   73   77   73   71   66   66   70   72   71   71
#> [241]   79   82   76   78   75   80   78   68   79   79   61   57   63   64   61
#> [256]   52   58   58   63   63   64   67   67   71   75   77   79   81   76   80
#> [271]   83   84   82   90   83   34   32   30   35   33   36   33   37   35   36
#> [286]   34   34   35   37   35   35   36   34   37   32   36   33   34   34   39
#> [301]   64   63   71   75   72   76   74   79   77   82   71   76   81   87   86
#> [316]   81   82   85   83   90   92   92   94   84   87   72   64   62   62   60
#> [331]   59   64   59   67   65   60   65   63   65   69   73   75   75   71   72
#> [346]   78   80   83   85   80   21   20   24   21   21   20   13   17   20   19
#> [361]   19   19   15   15   18   13   14   13   15   16   23   24   22   26   30
#> [376]   95  111  113  110  114  110  112  116  112  119  118  119  123  117  121
#> [391]  113  118  114  115  111  101  106  100   98  100  239  242  240  236  239
#> [406]  239  247  238  241  241  253  246  244  245  240  238  256  233  256  250
#> [421]  250  255  266  256  259   12   20   15   20   25   19   14   14   10   13
#> [436]   11   11   17   18   19   16   21   20   20   17   13   10   14   11    8
#> [451]  282  280  276  261  273  271  263  276  275  265  282  286  283  287  279
#> [466]  290  287  287  288  288  291  301  285  291  298   53   54   54   51   44
#> [481]   47   46   41   44   41   40   36   43   41   45   45   42   47   48   47
#> [496]   46   51   48   55   60   42   42   44   44   36   30   32   30   30   34
#> [511]   32   35   41   37   44   40   46   50   47   47   48   45   49   53   51
#> [526]   86   83   81   90   84   96  104   92   89   96   98   98  102   97  104
#> [541]  102  101   98   96   96  105   97  107  103  101   77   74   78   75   78
#> [556]   75   81   77   79   84   73   79   79   72   75   77   79   78   85   82
#> [571]   86   85   92   91   87   76   63   67   56   60   58   63   64   60   61
#> [586]   68   67   66   67   72   77   68   74   68   71   69   71   70   71   65
#> [601] 1744 1719 1764 1754 1751 1766 1730 1778 1780 1819 1829 1823 1860 1872 1918
#> [616] 1898 1957 1970 1980 1991 2001 1964 2045 2003 1997  253  246  247  217  233
#> [631]  207  315  230  204  198  305  327  284  313  308  235  233  333  229  223
#> [646]  227  200  211  237  255  269  354  306  289  245  304  229  273  240  333
#> [661]  337  372  272  233  253  234  267  305  227  275  308  296  222  240  239
#> [676]  297  358  256  241  332  360  256  322  302  317  310  298  295  330  281
#> [691]  283  241  238  340  268  289  330  250
#> 
#> $settings$stan_opts$data$y_smp
#>   [1]   19   20   16   13   13    8    9   10    5    4    4    4    4    4    6
#>  [16]    7   11   13   13   15   13   11   16   18   20   53   54   49   50   48
#>  [31]   53   52   53   57   55   51   55   55   56   60   62   65   66   66   64
#>  [46]   63   61   58   57   54   66   71   66   70   71   69   65   65   64   69
#>  [61]   71   70   70   70   69   68   72   69   69   65   65   62   65   65   62
#>  [76]   99   98   95   98  103  104  101  105  104  105  108  108  113  115  117
#>  [91]  119  117  119  117  121  124  120  124  126  126   89   91   90   94   95
#> [106]   95   97   97   93   91   90   89   87   89   94   93   94   98   99  100
#> [121]  100  102  106  110  114   61   63   59   54   53   53   55   55   54   57
#> [136]   58   63   58   56   58   61   57   59   61   60   63   62   59   58   56
#> [151]   61   65   62   63   63   61   56   59   58   63   60   56   56   55   58
#> [166]   55   54   57   61   65   61   58   54   55   57   49   52   53   57   58
#> [181]   59   60   60   64   68   64   68   66   65   62   64   66   70   70   68
#> [196]   67   64   67   65   60  373  372  376  374  374  375  378  379  381  383
#> [211]  386  389  386  384  380  385  388  389  392  387  387  386  390  387  382
#> [226]   85   80   80   83   82   83   83   81   77   79   76   81   82   79   82
#> [241]   85   90   85   83   84   88   86   82   85   84   72   74   79   77   75
#> [256]   71   76   74   72   74   78   80   78   82   85   89   90   88   87   91
#> [271]   92   91   94   98   98   37   34   34   38   36   39   37   39   38   40
#> [286]   38   37   40   40   37   36   37   37   38   35   38   38   38   36   41
#> [301]   85   83   86   87   89   91   91   89   90   91   90   90   95   97   94
#> [316]   94   92   93   97   97  101   99  100   97   97   75   72   68   65   65
#> [331]   64   65   68   70   67   65   68   69   70   74   75   77   78   74   79
#> [346]   84   84   88   87   84   29   26   31   26   24   22   17   20   24   20
#> [361]   21   22   19   19   22   20   16   14   18   20   24   27   22   27   32
#> [376]  117  122  122  124  126  126  129  126  126  131  130  133  132  132  128
#> [391]  125  123  125  124  120  115  112  111  110  111  276  279  279  277  278
#> [406]  275  279  277  275  279  281  276  272  271  270  269  272  274  278  280
#> [421]  279  283  287  285  283   17   20   19   23   26   22   17   16   11   14
#> [436]   15   13   17   21   22   19   23   23   24   19   14   11   15   13    9
#> [451]  338  341  339  335  332  330  332  328  328  327  331  331  331  329  331
#> [466]  327  329  328  327  328  328  331  331  335  334   62   67   65   62   57
#> [481]   55   51   50   47   47   44   41   45   47   52   53   51   54   59   54
#> [496]   50   54   56   61   66   44   47   48   45   40   35   34   33   35   37
#> [511]   35   39   43   44   45   48   51   54   49   52   53   49   53   57   55
#> [526]  115  114  110  113  114  118  123  119  119  120  120  120  122  117  120
#> [541]  118  118  113  117  113  116  118  120  115  117   92   92   91   88   88
#> [556]   91   90   92   92   95   92   92   87   82   84   86   89   92   95   97
#> [571]   96   94   97   96   92   79   74   70   66   70   65   68   66   66   68
#> [586]   72   72   72   72   76   80   76   78   74   77   75   74   75   72   70
#> [601] 2154 2170 2148 2144 2142 2128 2128 2125 2115 2146 2142 2157 2159 2156 2183
#> [616] 2194 2212 2228 2243 2242 2246 2229 2257 2264 2254  285  280  290  250  255
#> [631]  235  345  265  230  215  340  345  310  415  400  311  289  436  290  296
#> [646]  292  251  259  303  329  344  432  363  355  307  367  267  333  279  386
#> [661]  396  433  318  276  297  276  328  374  282  343  378  368  268  291  294
#> [676]  357  435  303  281  394  418  289  363  342  367  339  337  330  362  311
#> [691]  313  269  273  385  292  325  374  301
#> 
#> $settings$stan_opts$data$n_weights
#> [1] 750
#> 
#> $settings$stan_opts$data$obs_to_weights_bounds
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
#> $settings$stan_opts$data$weights_school
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
#> $settings$stan_opts$data$weights_cohort
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
#> $settings$stan_opts$data$weights_life_year
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
#> $settings$stan_opts$data$weights_dose
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
#> $settings$stan_opts$data$weights
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
#> $settings$stan_opts$data$n_cnty
#> [1] 3
#> 
#> $settings$stan_opts$data$cnty_bounds
#> [1]  1 11 18
#> 
#> $settings$stan_opts$data$predict_mode
#> [1] 0
#> 
#> 
#> $settings$stan_opts$object
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
#>   int<lower=0> y_obs[n_obs];
#>   int<lower=0> y_smp[n_obs];
#>   // have school id ranges for observations & for doses; school id 0 == statewide?
#>   // int obs_sch_id_bounds[n_obs];
#>   int<lower=n_obs> n_weights;
#>   array[n_obs] int<lower=1, upper=n_weights> obs_to_weights_bounds; // each entry is the start of the range
#>   int<lower=1,upper=n_sch + n_cnty + 1> weights_school[n_weights];
#>   int<lower=1,upper=n_cohort> weights_cohort[n_weights];
#>   int<lower=1,upper=n_yr> weights_life_year[n_weights];
#>   int<lower=1,upper=n_doses> weights_dose[n_weights];
#>   vector<lower=0,upper=1>[n_weights] weights; // contribution of this (school, cohort, year, dose) to an observation
#>   // run mode: 0 = estimation, 1 = prediction
#>   int<lower=0, upper=1> predict_mode;
#>   // TODO: calculate these in stan?
#>   // https://spinkney.github.io/helpful_stan_functions/group__splines.html
#>   // state-level basis spline
#>   int k_bs; // number of bspline basis functions
#>   matrix[n_cohort, k_bs] bs; // basis functions
#> # observations may be right-censored
#> # observation data is assumed ordered uncensored, then right censored
#> # so n_uncensored_obs == n_obs, all observations are uncensored
#> # number of uncensored observations
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
#>   int<lower=1> phi_lookup[n_weights];
#>   int<lower=1> cdf_lookup[n_weights];
#>   // because integer arrays don't support broadcasting ...
#>   // unroll phi and cdf objects to support vectorization
#>   for (weight_i in 1:n_weights) {
#>     // phi ordered by school then cohort
#>     phi_lookup[weight_i] = weights_cohort[weight_i] + (weights_school[weight_i] - 1) * n_cohort;
#>     // ordered by dose then life year
#>     cdf_lookup[weight_i] = weights_life_year[weight_i] + (weights_dose[weight_i] - 1) * n_yr;
#>   }
#> int<lower=-1> y_obs_trans[n_obs];
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
#> if (n_uncensored_obs < n_obs) { # at least some censored observations
#>     # p_s => 1 - p_s = p_f :: probability of at least this many successes =>
#>     #                         probability of less than this many failures
#>     if (n_uncensored_obs > 0) { # at least some uncensored observations
#>         target += binomial_lpmf(y_obs[:n_uncensored_obs] | y_smp[:n_uncensored_obs], p_obs[:n_uncensored_obs]);
#>     }
#>     target += binomial_lcdf(y_obs[(n_uncensored_obs+1):] | y_smp[(n_uncensored_obs+1):], 1 - p_obs[(n_uncensored_obs+1):]);
#> } else { # all uncensored observations
#>     y_obs ~ binomial(y_smp, p_obs); # vectorized
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
#>   [1]   16   14   14   10    8    7    6    9    5    4    4    4    4    4    6
#>  [16]    6    8   12   10   14   11   10   13   16   19   43   44   42   42   35
#>  [31]   42   41   43   51   48   44   43   44   46   54   56   54   57   57   54
#>  [46]   57   54   52   54   48   59   63   58   67   64   62   58   56   60   65
#>  [61]   65   65   64   67   62   64   67   64   61   63   62   57   65   60   60
#>  [76]   69   79   76   82   88   83   87   84   91   87   97   93  101  103   98
#>  [91]   96   97  103  102  106  112  105  111  112  108   71   73   73   85   83
#> [106]   87   82   82   79   77   79   80   75   83   85   88   86   90   91   92
#> [121]   89   93   96  101  108   58   59   58   45   44   48   52   52   49   47
#> [136]   52   59   54   54   55   54   55   55   54   52   57   57   53   52   50
#> [151]   50   51   48   53   51   48   46   52   52   55   54   50   45   47   49
#> [166]   49   50   53   53   62   48   53   49   47   47   46   50   49   54   54
#> [181]   50   54   57   58   57   61   64   58   57   56   61   63   63   62   64
#> [196]   59   59   65   65   55  283  288  278  279  280  279  290  290  308  309
#> [211]  290  325  297  318  305  321  318  320  338  337  340  319  330  333  332
#> [226]   68   61   65   72   68   73   77   73   71   66   66   70   72   71   71
#> [241]   79   82   76   78   75   80   78   68   79   79   61   57   63   64   61
#> [256]   52   58   58   63   63   64   67   67   71   75   77   79   81   76   80
#> [271]   83   84   82   90   83   34   32   30   35   33   36   33   37   35   36
#> [286]   34   34   35   37   35   35   36   34   37   32   36   33   34   34   39
#> [301]   64   63   71   75   72   76   74   79   77   82   71   76   81   87   86
#> [316]   81   82   85   83   90   92   92   94   84   87   72   64   62   62   60
#> [331]   59   64   59   67   65   60   65   63   65   69   73   75   75   71   72
#> [346]   78   80   83   85   80   21   20   24   21   21   20   13   17   20   19
#> [361]   19   19   15   15   18   13   14   13   15   16   23   24   22   26   30
#> [376]   95  111  113  110  114  110  112  116  112  119  118  119  123  117  121
#> [391]  113  118  114  115  111  101  106  100   98  100  239  242  240  236  239
#> [406]  239  247  238  241  241  253  246  244  245  240  238  256  233  256  250
#> [421]  250  255  266  256  259   12   20   15   20   25   19   14   14   10   13
#> [436]   11   11   17   18   19   16   21   20   20   17   13   10   14   11    8
#> [451]  282  280  276  261  273  271  263  276  275  265  282  286  283  287  279
#> [466]  290  287  287  288  288  291  301  285  291  298   53   54   54   51   44
#> [481]   47   46   41   44   41   40   36   43   41   45   45   42   47   48   47
#> [496]   46   51   48   55   60   42   42   44   44   36   30   32   30   30   34
#> [511]   32   35   41   37   44   40   46   50   47   47   48   45   49   53   51
#> [526]   86   83   81   90   84   96  104   92   89   96   98   98  102   97  104
#> [541]  102  101   98   96   96  105   97  107  103  101   77   74   78   75   78
#> [556]   75   81   77   79   84   73   79   79   72   75   77   79   78   85   82
#> [571]   86   85   92   91   87   76   63   67   56   60   58   63   64   60   61
#> [586]   68   67   66   67   72   77   68   74   68   71   69   71   70   71   65
#> [601] 1744 1719 1764 1754 1751 1766 1730 1778 1780 1819 1829 1823 1860 1872 1918
#> [616] 1898 1957 1970 1980 1991 2001 1964 2045 2003 1997  253  246  247  217  233
#> [631]  207  315  230  204  198  305  327  284  313  308  235  233  333  229  223
#> [646]  227  200  211  237  255  269  354  306  289  245  304  229  273  240  333
#> [661]  337  372  272  233  253  234  267  305  227  275  308  296  222  240  239
#> [676]  297  358  256  241  332  360  256  322  302  317  310  298  295  330  281
#> [691]  283  241  238  340  268  289  330  250
#> 
#> $data$y_smp
#>   [1]   19   20   16   13   13    8    9   10    5    4    4    4    4    4    6
#>  [16]    7   11   13   13   15   13   11   16   18   20   53   54   49   50   48
#>  [31]   53   52   53   57   55   51   55   55   56   60   62   65   66   66   64
#>  [46]   63   61   58   57   54   66   71   66   70   71   69   65   65   64   69
#>  [61]   71   70   70   70   69   68   72   69   69   65   65   62   65   65   62
#>  [76]   99   98   95   98  103  104  101  105  104  105  108  108  113  115  117
#>  [91]  119  117  119  117  121  124  120  124  126  126   89   91   90   94   95
#> [106]   95   97   97   93   91   90   89   87   89   94   93   94   98   99  100
#> [121]  100  102  106  110  114   61   63   59   54   53   53   55   55   54   57
#> [136]   58   63   58   56   58   61   57   59   61   60   63   62   59   58   56
#> [151]   61   65   62   63   63   61   56   59   58   63   60   56   56   55   58
#> [166]   55   54   57   61   65   61   58   54   55   57   49   52   53   57   58
#> [181]   59   60   60   64   68   64   68   66   65   62   64   66   70   70   68
#> [196]   67   64   67   65   60  373  372  376  374  374  375  378  379  381  383
#> [211]  386  389  386  384  380  385  388  389  392  387  387  386  390  387  382
#> [226]   85   80   80   83   82   83   83   81   77   79   76   81   82   79   82
#> [241]   85   90   85   83   84   88   86   82   85   84   72   74   79   77   75
#> [256]   71   76   74   72   74   78   80   78   82   85   89   90   88   87   91
#> [271]   92   91   94   98   98   37   34   34   38   36   39   37   39   38   40
#> [286]   38   37   40   40   37   36   37   37   38   35   38   38   38   36   41
#> [301]   85   83   86   87   89   91   91   89   90   91   90   90   95   97   94
#> [316]   94   92   93   97   97  101   99  100   97   97   75   72   68   65   65
#> [331]   64   65   68   70   67   65   68   69   70   74   75   77   78   74   79
#> [346]   84   84   88   87   84   29   26   31   26   24   22   17   20   24   20
#> [361]   21   22   19   19   22   20   16   14   18   20   24   27   22   27   32
#> [376]  117  122  122  124  126  126  129  126  126  131  130  133  132  132  128
#> [391]  125  123  125  124  120  115  112  111  110  111  276  279  279  277  278
#> [406]  275  279  277  275  279  281  276  272  271  270  269  272  274  278  280
#> [421]  279  283  287  285  283   17   20   19   23   26   22   17   16   11   14
#> [436]   15   13   17   21   22   19   23   23   24   19   14   11   15   13    9
#> [451]  338  341  339  335  332  330  332  328  328  327  331  331  331  329  331
#> [466]  327  329  328  327  328  328  331  331  335  334   62   67   65   62   57
#> [481]   55   51   50   47   47   44   41   45   47   52   53   51   54   59   54
#> [496]   50   54   56   61   66   44   47   48   45   40   35   34   33   35   37
#> [511]   35   39   43   44   45   48   51   54   49   52   53   49   53   57   55
#> [526]  115  114  110  113  114  118  123  119  119  120  120  120  122  117  120
#> [541]  118  118  113  117  113  116  118  120  115  117   92   92   91   88   88
#> [556]   91   90   92   92   95   92   92   87   82   84   86   89   92   95   97
#> [571]   96   94   97   96   92   79   74   70   66   70   65   68   66   66   68
#> [586]   72   72   72   72   76   80   76   78   74   77   75   74   75   72   70
#> [601] 2154 2170 2148 2144 2142 2128 2128 2125 2115 2146 2142 2157 2159 2156 2183
#> [616] 2194 2212 2228 2243 2242 2246 2229 2257 2264 2254  285  280  290  250  255
#> [631]  235  345  265  230  215  340  345  310  415  400  311  289  436  290  296
#> [646]  292  251  259  303  329  344  432  363  355  307  367  267  333  279  386
#> [661]  396  433  318  276  297  276  328  374  282  343  378  368  268  291  294
#> [676]  357  435  303  281  394  418  289  363  342  367  339  337  330  362  311
#> [691]  313  269  273  385  292  325  374  301
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
