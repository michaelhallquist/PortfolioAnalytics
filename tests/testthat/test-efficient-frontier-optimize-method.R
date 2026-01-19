with_pa_mocked_bindings <- function(mocks, code) {
  ns <- asNamespace("PortfolioAnalytics")
  orig <- mget(names(mocks), envir = ns, inherits = FALSE)

  on.exit({
    for (nm in names(orig)) {
      unlockBinding(nm, ns)
      assign(nm, orig[[nm]], envir = ns)
      lockBinding(nm, ns)
    }
  }, add = TRUE)

  for (nm in names(mocks)) {
    unlockBinding(nm, ns)
    assign(nm, mocks[[nm]], envir = ns)
    lockBinding(nm, ns)
  }

  eval(substitute(code), envir = parent.frame())
}

test_that("create.EfficientFrontier passes optimize_method through to meanvar", {

  foreach::registerDoSEQ()

  assets <- c("A", "B")
  portf <- portfolio.spec(assets = assets)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")

  dates <- as.Date("2020-01-01") + 0:2
  mat <- cbind(
    A = c(0.01, 0.02, 0.03),
    B = c(0.02, 0.01, 0.00)
  )
  returns <- xts::xts(mat, order.by = dates)

  seen_methods <- character(0)
  mock_opt <- function(R, portfolio, optimize_method, ...) {
    seen_methods <<- c(seen_methods, optimize_method[1])
    w <- rep(1 / length(portfolio$assets), length(portfolio$assets))
    names(w) <- names(portfolio$assets)
    structure(list(weights = w, out = 0), class = c("optimize.portfolio.ROI", "optimize.portfolio"))
  }
  mock_stats <- function(object, ...) {
    c(mean = 0, StdDev = 0, out = 0)
  }

  with_pa_mocked_bindings(
    list(optimize.portfolio = mock_opt, extractStats = mock_stats),
    {
      create.EfficientFrontier(
        R = returns,
        portfolio = portf,
        type = "mean-var",
        optimize_method = "ROI",
        n.portfolios = 3
      )

      expect_true(all(seen_methods == "ROI"))
    }
  )
})

test_that("extractEfficientFrontier passes optimize_method for ROI objects", {
  foreach::registerDoSEQ()

  assets <- c("A", "B")
  portf <- portfolio.spec(assets = assets)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  portf <- add.objective(portf, type = "return", name = "mean")

  dates <- as.Date("2020-01-01") + 0:2
  mat <- cbind(
    A = c(0.01, 0.02, 0.03),
    B = c(0.02, 0.01, 0.00)
  )
  returns <- xts::xts(mat, order.by = dates)

  seen_methods <- character(0)
  mock_meanvar <- function(portfolio, R, optimize_method, ...) {
    seen_methods <<- c(seen_methods, optimize_method[1])
    structure(matrix(0, nrow = 1), class = "frontier")
  }

  obj <- structure(
    list(portfolio = portf, R = returns, weights = c(A = 0.5, B = 0.5), out = 0),
    class = c("optimize.portfolio.ROI", "optimize.portfolio")
  )

  with_pa_mocked_bindings(
    list(meanvar.efficient.frontier = mock_meanvar),
    {
      extractEfficientFrontier(obj, match.col = "StdDev", optimize_method = "ROI", n.portfolios = 3)
      expect_equal(seen_methods, "ROI")
    }
  )
})
