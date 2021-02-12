test_that("main_p2() only depends on sufficient statistics", {
  set.seed(1)
  nm1  <- sim_p(c(0.5, 0.1), n = 100)
  nm2 <- sample(nm1)

  out1 <- main_p2(nm = nm1)
  out2 <- main_p2(nm = nm2)

  expect_equal(
    out1,
    out2,
    tolerance = 10^-4
  )
})

test_that("main_p3() only depends on sufficient statistics", {
  set.seed(1)
  nm1  <- sim_p3(c(0.5, 0.1), n = 100)
  nm2 <- sample(nm1)

  out1 <- main_p3(nm = nm1)
  out2 <- main_p3(nm = nm2)

  expect_equal(
    out1,
    out2,
    tolerance = 10^-4
  )
})
