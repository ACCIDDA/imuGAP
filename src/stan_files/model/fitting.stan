
// school-level observations
for(i in 1:n_sch_obs) {
  y_sch[i, 1] ~ binomial(y_sch[i, 2], inv_logit(lambda[y_sch_id[i], y_sch_yr[i]]));
}

// state-level coverages for each year for the given age group
vector[n_yr] lambda_s[3];
for(i in 1:3) {
    lambda_s[i] = diagonal((lambda_c')*pop_wts[i])
}

// state-level observations
for(i in 1:n_state_obs) {

  y_state[i, 1] ~ binomial(y_state[i, 2], inv_logit((y_state_grades[i,]*lambda_s[y_state_pop[i]])/y_state_grade_rowsum[i]));

}