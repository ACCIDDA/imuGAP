
  // offsets as normal
  // TODO, figure out how to: #include "./parameters/cnty_sch.stan"
  real<lower=0> sigma_cnty;
  row_vector<multiplier=sigma_cnty>[n_cnty] off_cnty;

  real<lower=0> sigma_sch;
  row_vector<multiplier=sigma_sch>[n_sch] off_sch;

  // introduce trend terms
  real<lower=0> sigma_trend_cnty;
  row_vector<multiplier=sigma_trend_cnty>[n_cnty] trend_cnty;

  real<lower=0> sigma_trend_sch;
  row_vector<multiplier=sigma_trend_sch>[n_sch] trend_sch;
