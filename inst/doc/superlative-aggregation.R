## -----------------------------------------------------------------------------
library(piar)

set.seed(12345)

# Make 6 elemental indexes over 4 time periods.
elementals <- matrix(c(rep(1, 6), runif(6 * 3, 0.8, 1.2)), nrow = 6) |>
  as_index() |>
  set_levels(paste0("B", 1:6))

head(elementals)

# Make aggregation weights over 4 time periods.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4  B5  B6

weights <- data.frame(
  level1 = 1,
  level2 = rep(11:12, each = 3),
  ea = levels(elementals),
  weights = runif(4 * 6, 100, 200),
  period = rep(1:4, each = 6)
)

head(weights)

## -----------------------------------------------------------------------------
elementals <- unstack(elementals)

## -----------------------------------------------------------------------------
paasche_pias <- split(
  weights[c("level1", "level2", "ea", "weights")],
  weights[["period"]]
) |>
  lapply(as_aggregation_structure)

## -----------------------------------------------------------------------------
paasche <- Map(
  aggregate,
  elementals,
  paasche_pias,
  na.rm = TRUE,
  include_ea = FALSE,
  r = -1
) |>
  Reduce(stack, x = _)

paasche

## -----------------------------------------------------------------------------
laspeyres_pias <- paasche_pias[c(1, 1, 2, 3)]

fisher <- Map(
  aggregate,
  elementals,
  pias = laspeyres_pias,
  pias2 = paasche_pias,
  na.rm = TRUE,
  include_ea = FALSE
) |>
  Reduce(stack, x = _)

fisher

## -----------------------------------------------------------------------------
laspeyres <- Map(
  aggregate,
  elementals,
  pias = laspeyres_pias,
  na.rm = TRUE,
  include_ea = FALSE
) |>
  Reduce(stack, x = _)

sqrt(as.matrix(laspeyres) * as.matrix(paasche))

