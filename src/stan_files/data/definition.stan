
data {
  int n_yr; // number of years (grade cohorts) 
  int n_cnty; // number of counties
  int n_school; // number of counties
  int n_state_obs; // number of state-level coverage observations
  int n_sch_obs; // number of school-level coverage observations
  
  // Defines chunks of schools in the same county
  int cnty_start_index[n_cnty];
  int cnty_end_index[n_cnty];
  
  // weights
  matrix[n_school, n_yr] sch_wts; // school weights = enrollment at school/total enrollment in county
  matrix[n_cnty, n_yr] pop_wts[3]; // county population weights = county pop for age group/state pop for age group for 19-35 mos (1), kindergarten entry (2), and 13-15 yos (3)
  
  // school-level observations
  int y_sch[n_sch_obs, 2]; // observed school-level num up to date (col 1) and total enrollment (col 2)
  int y_sch_id[n_sch_obs]; // school IDs
  int y_sch_yr[n_sch_obs]; // years
  
  // state-level observations
  int y_state[n_state_obs, 2]; // state-level coverage observations num up to date (col 1) and total enrollment (col 2)
  matrix[n_state_obs, n_yr] y_state_grades; // weights for grade cohorts corresponding to state-level observations
  int y_state_pop[n_state_obs]; // 19-35 mos (1), kindergarten entry (2), and 13-15 yos (3)
}