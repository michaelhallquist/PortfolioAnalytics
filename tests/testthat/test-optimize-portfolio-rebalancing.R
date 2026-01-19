test_that("optimize.portfolio.rebalancing trims assets without turnover constraint", {
  skip_if_not_installed("iterators")
  skip_if_not_installed("foreach")

  foreach::registerDoSEQ()

  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "return", name = "mean")

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  out <- optimize.portfolio.rebalancing(
    R = returns,
    portfolio = portf,
    optimize_method = "random",
    search_size = 10,
    rebalance_on = "months",
    training_period = 1,
    mincov = 3
  )

  opt <- out$opt_rebalancing[[1]]
  expect_equal(names(opt$portfolio$assets), c("A", "C"))
  expect_equal(names(opt$weights), c("A", "C"))
})

test_that("optimize.portfolio.rebalancing trims assets with turnover constraint", {
  skip_if_not_installed("iterators")
  skip_if_not_installed("foreach")

  foreach::registerDoSEQ()

  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.constraint(portf, type = "turnover", turnover_target = 0.1)
  portf <- add.objective(portf, type = "return", name = "mean")

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  out <- optimize.portfolio.rebalancing(
    R = returns,
    portfolio = portf,
    optimize_method = "random",
    search_size = 10,
    rebalance_on = "months",
    training_period = 1,
    mincov = 3
  )

  opt <- out$opt_rebalancing[[1]]
  expect_equal(names(opt$portfolio$assets), c("A", "C"))
  expect_equal(names(opt$weights), c("A", "C"))

  turnover_idx <- which(sapply(opt$portfolio$constraints, function(x) x$type == "turnover"))
  expect_equal(length(turnover_idx), 1L)
  weight_initial <- opt$portfolio$constraints[[turnover_idx[1]]]$weight_initial
  expect_equal(names(weight_initial), c("A", "C"))
  expect_equal(length(weight_initial), 2L)
})
