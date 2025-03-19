## -----------------------------------------------------------------------------
library(piar)

set.seed(12345)

# Make quarterly elemental indexes for two baskets with a 1 year overlap.

elementals1 <- matrix(
  runif(5 * 8, 0.8, 1.2),
  nrow = 5,
  dimnames = list(paste0("B", 1:5), NULL)
)|>
  as_index()

elementals2 <- matrix(
  runif(6 * 8, 0.8, 1.2), 
  nrow = 6,
  dimnames = list(paste0("B", 1:6), 5:12)
) |>
  as_index() 

# Make aggregation weights for basket 1.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4      B5

weights1 <- data.frame(
  level1 = 1,
  level2 = c(11, 11, 11, 12, 12),
  ea = levels(elementals1),
  weights = runif(5, 100, 200)
)

# Make aggregation weights for basket 2.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4  B5  B6

weights2 <- data.frame(
  level1 = 1,
  level2 = c(11, 11, 11, 12, 12, 12),
  ea = levels(elementals2),
  weights = runif(6, 100, 200)
)

## -----------------------------------------------------------------------------
index <- Map(
  aggregate,
  list(elementals1, elementals2),
  list(weights1, weights2),
  include_ea = FALSE
)

## -----------------------------------------------------------------------------
stack(index[[1]], window(index[[2]], start = "9")) |>
  chain()

## -----------------------------------------------------------------------------
link_factor <- chain(index[[1]])|>
  window(start = end(index[[1]])) |>
  as.numeric()

stack(
  chain(index[[1]]),
  chain(window(index[[2]], start = "9"), link = link_factor)
)

## -----------------------------------------------------------------------------
index <- index |>
  lapply(chain) |>
  lapply(\(x) rebase(x, mean(x[, 1:4])))

## -----------------------------------------------------------------------------
link_factor <- window(index[[1]], start = "5") |>
  mean() |>
  as.numeric()

stack(
  index[[1]],
  rebase(window(index[[2]], start = "9"), base = 1 / link_factor)
)

## -----------------------------------------------------------------------------
index[[1]] <- rebase(index[[1]], mean(window(index[[1]], start = "5")))

link_factor = as.numeric(index[[1]][, "8"]) / as.numeric(index[[2]][, "8"])

stack(
  index[[1]],
  rebase(window(index[[2]], start = "9"), base = 1 / link_factor)
)

