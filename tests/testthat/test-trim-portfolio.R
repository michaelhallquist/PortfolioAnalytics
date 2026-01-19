test_that("trim_portfolio returns original when no assets are dropped", {
  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "return", name = "mean")

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.02, 0.01, 0.00, -0.01, 0.03),
    C = c(-0.01, 0.00, 0.01, 0.02, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  res <- trim_portfolio(portf, returns, mincov = 3)
  expect_identical(res, portf)
})

test_that("trim_portfolio respecs when assets are dropped", {
  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  min_val <- 0
  max_val <- 1
  portf <- add.constraint(portf, type = "box", min = min_val, max = max_val)
  portf <- add.objective(portf, type = "return", name = "mean")
  rm(min_val, max_val)

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  res <- trim_portfolio(portf, returns, mincov = 3)
  expect_equal(names(res$assets), c("A", "C"))
  expect_equal(length(res$constraints), length(portf$constraints))
  expect_equal(length(res$objectives), length(portf$objectives))
})

test_that("trim_portfolio remaps group constraints after asset drops", {
  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  portf <- add.constraint(
    portf,
    type = "group",
    groups = list(group1 = c(1, 2), group2 = c(2), group3 = c(3)),
    group_min = c(0.1, 0.2, 0.3),
    group_max = c(0.5, 0.6, 0.7),
    group_pos = c(1, 1, 1)
  )
  portf$constraints[[1]]$call <- NULL

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  res <- trim_portfolio(portf, returns, mincov = 3)
  grp <- res$constraints[[1]]
  expect_equal(names(grp$groups), c("group1", "group3"))
  expect_equal(grp$groups$group1, 1)
  expect_equal(grp$groups$group3, 2)
  expect_equal(grp$cLO, c(0.1, 0.3))
  expect_equal(grp$cUP, c(0.5, 0.7))
  expect_equal(grp$group_pos, c(1, 1))
  expect_equal(grp$group_labels, c("group1", "group3"))
})

test_that("trim_portfolio remaps named group constraints after asset drops", {
  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  portf <- add.constraint(
    portf,
    type = "group",
    groups = list(group1 = c("A", "B"), group2 = "C"),
    group_min = c(0.1, 0.2),
    group_max = c(0.5, 0.6)
  )
  portf$constraints[[1]]$call <- NULL

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  res <- trim_portfolio(portf, returns, mincov = 3)
  grp <- res$constraints[[1]]
  expect_equal(names(grp$groups), c("group1", "group2"))
  expect_equal(grp$groups$group1, 1)
  expect_equal(grp$groups$group2, 2)
})

test_that("trim_portfolio trims factor exposure matrices", {
  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  B <- matrix(
    c(1, 0, 0, 1, 0.5, 0.5),
    nrow = 3,
    dimnames = list(assets, c("F1", "F2"))
  )
  portf <- add.constraint(
    portf,
    type = "factor_exposure",
    B = B,
    lower = c(-1, -1),
    upper = c(1, 1)
  )
  portf$constraints[[1]]$call <- NULL

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  res <- trim_portfolio(portf, returns, mincov = 3)
  fe <- res$constraints[[1]]
  expect_equal(rownames(fe$B), c("A", "C"))
  expect_equal(nrow(fe$B), 2L)
  expect_equal(colnames(fe$B), c("F1", "F2"))
})

test_that("trim_portfolio trims objective arguments with asset vectors", {
  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  portf <- add.objective(
    portf,
    type = "return",
    name = "mean",
    arguments = list(
      weights = c(A = 0.2, B = 0.3, C = 0.5),
      alpha = c(1, 2, 3)
    )
  )
  portf$objectives[[1]]$call <- NULL

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  res <- trim_portfolio(portf, returns, mincov = 3)
  obj <- res$objectives[[1]]
  expect_equal(names(obj$arguments$weights), c("A", "C"))
  expect_equal(obj$arguments$weights, c(A = 0.2, C = 0.5))
  expect_equal(obj$arguments$alpha, c(1, 3))
})

test_that("trim_portfolio rebuilds turnover constraint and objectives", {
  assets <- c("A", "B", "C")
  portf <- portfolio.spec(assets = assets)
  portf <- add.constraint(
    portf,
    type = "turnover",
    turnover_target = 0.2,
    weight_initial = c(A = 0.2, B = 0.3, C = 0.5)
  )
  portf <- add.objective(
    portf,
    type = "risk_budget",
    name = "StdDev",
    min_prisk = c(A = 0.1, B = 0.2, C = 0.3),
    max_prisk = c(A = 0.5, B = 0.6, C = 0.7),
    target = 0.05,
    multiplier = 2,
    enabled = FALSE
  )
  portf <- add.objective(
    portf,
    type = "return",
    name = "mean",
    target = 0.01,
    multiplier = 3,
    enabled = FALSE
  )

  dates <- as.Date("2020-01-01") + 0:4
  mat <- cbind(
    A = c(0.01, 0.02, 0.03, 0.04, 0.05),
    B = c(0.01, NA, NA, NA, 0.02),
    C = c(0.02, 0.01, 0.00, -0.01, 0.03)
  )
  returns <- xts::xts(mat, order.by = dates)

  res <- trim_portfolio(portf, returns, mincov = 3)
  turnover_idx <- which(sapply(res$constraints, function(x) inherits(x, "turnover_constraint")))
  expect_equal(length(turnover_idx), 1L)
  turnover <- res$constraints[[turnover_idx]]
  expect_equal(names(turnover$weight_initial), c("A", "C"))
  expect_equal(turnover$weight_initial, c(A = 0.2, C = 0.5))

  rb_idx <- which(sapply(res$objectives, function(x) inherits(x, "risk_budget_objective")))
  expect_equal(length(rb_idx), 1L)
  rb <- res$objectives[[rb_idx]]
  expect_equal(names(rb$min_prisk), c("A", "C"))
  expect_equal(rb$min_prisk, c(A = 0.1, C = 0.3))
  expect_equal(names(rb$max_prisk), c("A", "C"))
  expect_equal(rb$max_prisk, c(A = 0.5, C = 0.7))
  expect_false(rb$enabled)
  expect_equal(rb$target, 0.05)
  expect_equal(rb$multiplier, 2)

  ret_idx <- which(sapply(res$objectives, function(x) inherits(x, "return_objective")))
  expect_true(length(ret_idx) >= 1L)
  ret_obj <- res$objectives[[ret_idx[1]]]
  expect_false(ret_obj$enabled)
  expect_equal(ret_obj$target, 0.01)
  expect_equal(ret_obj$multiplier, 3)
})
