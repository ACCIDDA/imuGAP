# imuGAP Model Options

This function encapsulates option passing for imuGAP settings.

## Usage

``` r
imugap_options(df = 5L, dose_schedule = c(1, 4), object = c("default"))
```

## Arguments

- df:

  degrees of freedom to use in bspline

- dose_schedule:

  an integer vector, the ages at which dose(s) `n` are scheduled, with
  vector indices and doses matching

- object:

  which stan model object to use; currently only "default" is supported

## Value

a list of imuGAP model options

## Examples

``` r
imugap_options()
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 4
#> 
#> $object
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
imugap_options(dose_schedule = c(1, 3))
#> $df
#> [1] 5
#> 
#> $dose_schedule
#> [1] 1 3
#> 
#> $object
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
```
