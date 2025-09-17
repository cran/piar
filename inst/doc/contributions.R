## -----------------------------------------------------------------------------
library(piar)

# Make an aggregation structure.
ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

# Make elementary index with contributions.
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(
    relative ~ period + business,
    product = product,
    na.rm = TRUE,
    contrib = TRUE
  )

## -----------------------------------------------------------------------------
contrib(elementals, level = "B1")

## -----------------------------------------------------------------------------
contrib2DF(elementals, level = "B1")

## -----------------------------------------------------------------------------
index <- aggregate(elementals, pias, na.rm = TRUE)

contrib(index)

## -----------------------------------------------------------------------------
index <- as_index(as.matrix(index), contrib = TRUE)

## -----------------------------------------------------------------------------
set_contrib_from_index(index) |>
  aggregate(cut(pias, 2)) |>
  contrib()

## -----------------------------------------------------------------------------
chain(index) |>
  set_contrib_from_index() |>
  aggregate(cut(pias, 2)) |>
  contrib()

