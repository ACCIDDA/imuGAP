vector[n_cohort] logit_phi_st = bs * beta_bs;
vector[n_cohort] phi = inv_logit(logit_phi_st);
vector[n_doses * n_yr] unrolled_dose_probs = unrolled_dose(n_yr, n_doses, dose_sched, lambda_raw, epsilon_p);

#include model/common_phi.stan
