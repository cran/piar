## -----------------------------------------------------------------------------
set.seed(54321)

library(piar)

# Make an aggregation structure.
#                  1
#      |-----------|-----------|
#      11          12          13
#  |---+---|   |---+---|   |---+---|
# 111     121 121     122 131     132

pias <- data.frame(
  level1 = rep(1, 12),
  level2 = rep(c(11, 12, 13), each = 4),
  level3 = rep(c(111, 112, 121, 122, 131, 132), each = 2),
  ea = sprintf("B%02d", 1:12),
  weight = 1:12
) |>
  as_aggregation_structure()

pias

# Make elementary indexes over 3 years and aggregate.
quarterly_index <- matrix(
  runif(12 * 12, 0.4, 1.2),
  nrow = 12,
  dimnames = list(sprintf("B%02d", 1:12), paste0("Q", 1:12))
) |>
  as_index() |>
  aggregate(pias)

head(quarterly_index)

## -----------------------------------------------------------------------------
annual_index <- chain(quarterly_index) |>
  mean(window = 4)

annual_index |>
  rebase("Q1") |>
  head()

## -----------------------------------------------------------------------------
annual_pias <- pias |>
  update(annual_index, period = "Q1")

annual_index <- annual_index |>
  rebase("Q1")

annual_index |>
  aggregate(annual_pias) |>
  all.equal(annual_index)

## -----------------------------------------------------------------------------
annual_index |>
  unchain() |>
  set_contrib_from_index() |>
  aggregate(cut(annual_pias, 2)) |>
  contrib()

