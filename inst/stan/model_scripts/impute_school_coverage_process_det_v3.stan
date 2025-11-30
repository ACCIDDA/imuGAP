#include imugap_functions.stan

// need meta info:
//  which cohorts, which places, which years of life
//  => assume each measurement is independent, not progress of some matched individuals

data {

#include stan/data/shared.stan
#include stan/data/bspline.stan

}

#include stan/data/transformed.stan

parameters {

  #include stan/parameters/bspline.stan
  // #include stan/parameters/constant_phi.stan

  // #include stan/parameters/cnty_sch_linear.stan
  #include stan/parameters/cnty_sch.stan
  
  #include stan/parameters/static_lambda.stan

}

transformed parameters {
  // state-level phi by year, constructed from beta spline
  // Construct state-level trend from basis functions
  
  #include stan/transformed_parameters/bspline.stan // yields logit_phi_st
  // #include stan/transformed_parameters/constant_phi.stan

  #include stan/transformed_parameters/static_lambda.stan // yields unrolled dose cdf

  #include stan/transformed_parameters/p_obs.stan // yields p_obs

}

model {

  if (!predict_mode) {

    // #include stan/model/cnty_sch_linear.stan
    #include stan/model/cnty_sch.stan
    #include stan/model/bspline.stan
    // #include stan/model/constant_phi.stan
    #include stan/model/static_lambda.stan
    #include stan/model/shared.stan

  }

}

#include stan/generated_quantities/shared.stan

// This model represents vaccination as a discrete step, fixed hazard process
// therefore X(t) => X(t + deltaT) = X(t)*exp(-hazard*deltaT)
// which means the X-out flow == X(t)*(1-exp(-hazard*deltaT))
//
// for an arbitrary number of hazards & deltaTs, we simply sum:
// net outflow: X(t)*(1-exp(-sum(hazard*deltaT)))
//
// This model represents an overall hazard, shared across birth cohorts and
// vaccine doses. Additionally, birth cohorts have a propensity to vaccinate,
// which is fixed at birth and then applies throughout life.
//
// deltaT captures both time passage + eligibility. deltaT in the fitting is
// always == 1, but note that this is unitless - the data define what time means
// e.g. if birth cohorts are based on years, then deltaT is years, and a weight
// of 1 means eligible for a whole year, .25 eligible for a quarter, etc
//
// the hazard is constant for any given discrete time step, but can vary between
// time steps. the hazard time is denoted in absolute model time. however,
// cohort time is relative. so:
// hazard time == 1: one deltaT has passed, absolute time is 1
// cohort 1, time 1: one deltaT has passed => cohort 1 is 1 deltaT old,
//   absolute time is also 1 (cohort 1 is the first cohort)
// cohort 2, time 2: two deltaT have passed => cohort 2 is 2 deltaT old, but
//   cohort 1 is 3 deltaT old, and absolute time is 3
// generally:
// cohort n, time t: experienced t deltaTs; those deltaT corresponded to absolute
//   times n, n+1, ..., n+t
//
// we assume each cohort has the same doses schedule, so the same deltaT weights
//
// so for cohort n, fraction of (non-phi/vaccinators) with 1+ doses at age 1:t:
//   d1cdf = 1 - exp(-cumsum(weight[1:t] .* lambda[n:t]))
//
// and the incidence of new dose 1s is:
//
//   c(d1cdf[1], diff(d1cdf))
//
// for subsequent doses, the hazard model is the same, but now is conditional on
// having received the first dose; thus the incremental hazard is still
//   d2cdf = 1 - exp(-cumsum(weight2[1:t] .* lambda[n:t]))
//   (note the different eligibility mask, weight2; assert weight2 strictly less than weight1)
// but only the fraction having dose 1 experience it.
// if we think about this in terms of newly-dose-1-in-deltaT-x:
//   we can reframe in these terms:
//   define d2pdf = c(d2cdf[1], diff(d2cdf)); pgtt = 1-d2cdf[t] (probably event has not occured by t)
//   probability of dose 2 in year (x+1):t => d2pdf[(x+1):t]/sum(d2pdf[x+1:t] + pggt)
//   we can then assign each incremental incidence of dose 1 in deltaT x to getting
//   second dose in deltaT x+1:t (or not getting it by t)
//
// this can be built-up as a matrix once per cohort
//  1. calculate d2cdf, d2pdf, pgtt as above, for t == max cohort "age"
//  2. calculate remd2pdf = rev(cumsum(rev(d2pdf))) + pgtt
//  3. d2contrib =
//    upper_triangle_1s * colwise element multiplication of d2pdf / rowwise remd2pdf * rowwise d2incidence => column sums => proportion in d2
