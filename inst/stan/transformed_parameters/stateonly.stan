
  // school-level phi by year, converted to column major vector
  // so it's school n, cohort 1 => n_cohort; school n+1, ...
  // TODO double check mapping from matrix to vector
  vector[n_cohort] phi = inv_logit(logit_phi_st);
