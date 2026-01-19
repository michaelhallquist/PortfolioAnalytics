test_that("reduce_matrix_mincov clamps mincov to available observations", {
  mat <- matrix(
    c(0.01, 0.02, 0.03, 0.04),
    nrow = 2,
    dimnames = list(NULL, c("A", "B"))
  )
  res <- expect_warning(
    reduce_matrix_mincov(mat, mincov = 5, verbose = FALSE),
    "mincov exceeds available observations"
  )
  expect_equal(dim(res), c(2L, 2L))
  expect_null(attr(res, "dropped_assets"))
})

test_that("reduce_matrix_mincov retains assets with exactly mincov observations", {
  mat <- matrix(
    c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
    nrow = 3,
    dimnames = list(NULL, c("A", "B"))
  )
  res <- reduce_matrix_mincov(mat, mincov = 3, verbose = FALSE)
  expect_equal(dim(res), c(3L, 2L))
  expect_null(attr(res, "dropped_assets"))
})

test_that("trim_portfolio errors when all assets are dropped", {
  assets <- c("A", "B")
  portf <- portfolio.spec(assets = assets)
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "return", name = "mean")

  dates <- as.Date("2020-01-01") + 0:2
  mat <- cbind(
    A = c(0.01, NA, NA),
    B = c(NA, 0.02, NA)
  )
  returns <- xts::xts(mat, order.by = dates)

  expect_warning(
    expect_error(
      trim_portfolio(portf, returns, mincov = 3),
      "All assets were dropped"
    ),
    "All assets dropped due to insufficient observations"
  )
})

test_that("reduce_matrix_mincov handles single-column matrices", {
  mat <- matrix(
    c(0.01, 0.02, NA, 0.04),
    ncol = 1,
    dimnames = list(NULL, "A")
  )
  res <- reduce_matrix_mincov(mat, mincov = 2, verbose = FALSE)
  expect_equal(dim(res), c(4L, 1L))
  expect_null(attr(res, "dropped_assets"))
})

test_that("reduce_matrix_mincov warns on zero observations", {
  mat <- matrix(numeric(0), nrow = 0, ncol = 2, dimnames = list(NULL, c("A", "B")))
  res <- expect_warning(
    reduce_matrix_mincov(mat, mincov = 1, verbose = FALSE),
    "No observations available"
  )
  expect_equal(dim(res), c(0L, 2L))
  expect_null(attr(res, "dropped_assets"))
})
