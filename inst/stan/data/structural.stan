int<lower=1> n_yr; // number of years to model for each cohort - should be at least year of oldest observation
int<lower=1> n_cohort; // number of birth year cohorts

// dose schedules
int<lower=1> n_doses;
matrix<lower=0, upper=1>[n_yr, n_doses] dose_sched;

// run mode: 0 = estimation, 1 = prediction
int<lower=0, upper=1> predict_mode;
