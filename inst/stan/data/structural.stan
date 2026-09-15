int<lower=1> n_yr; // number of years to model for each cohort - should be at least year of oldest observation
int<lower=1> n_cohort; // number of birth year cohorts

// dose schedules and evaluation intervals
int<lower=1> n_intervals;
vector<lower=0>[n_intervals] dt_vec;
int<lower=1> n_doses;
matrix<lower=0, upper=1>[n_intervals, n_doses] dose_sched;
array[n_yr] int<lower=1, upper=n_intervals> age_to_interval_map;

// run mode: 0 = estimation, 1 = prediction
int<lower=0, upper=1> predict_mode;
