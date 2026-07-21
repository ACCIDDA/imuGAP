vector[n_sch] school_w_in_cnty;
vector[n_cnty] cnty_w_in_state;
real total_state_enrollment = sum(school_enrollment);

for (c in 1:n_cnty) {
  real cnty_sum = sum(school_enrollment[cnty_map[1, c]:cnty_map[2, c]]);
  school_w_in_cnty[cnty_map[1, c]:cnty_map[2, c]] = 
    school_enrollment[cnty_map[1, c]:cnty_map[2, c]] / cnty_sum;
  cnty_w_in_state[c] = cnty_sum / total_state_enrollment;
}
