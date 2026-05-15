library(data.table)

make_test_locs <- function() {
  data.frame(
    loc_id = c("state", "cnty", "schl"),
    parent_id = c(NA, "state", "cnty")
  )
}

make_test_obs <- function() {
  data.frame(
    obs_id = c("o1", "o2"),
    positive = c(5L, 10L),
    sample_n = c(10L, 20L)
  )
}

make_test_pops <- function() {
  data.table(
    obs_id = c("o1", "o2"),
    loc_id = c("schl", "schl"),
    cohort = c(1L, 1L),
    age = c(2L, 2L),
    dose = c(1L, 2L),
    weight = c(1.0, 1.0)
  )
}
