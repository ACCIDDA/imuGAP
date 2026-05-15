extract_imugap <- imuGAP:::extract_imugap

test_that("extract_imugap errors on non-stanfit input", {
  expect_error(extract_imugap(list()))
  expect_error(extract_imugap(NULL))
  expect_error(extract_imugap("not a stanfit"))
  expect_error(extract_imugap(data.frame(x = 1)))
})
