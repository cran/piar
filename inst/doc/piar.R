## -----------------------------------------------------------------------------
library(piar)

head(ms_prices)

ms_weights

## -----------------------------------------------------------------------------
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)

elementals

## -----------------------------------------------------------------------------
elementals[, "202004"]

elementals[c("B1", "B3"), ]

## -----------------------------------------------------------------------------
ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

## -----------------------------------------------------------------------------
index <- aggregate(elementals, pias, na.rm = TRUE)

index

## -----------------------------------------------------------------------------
chained_index <- chain(index)

chained_index

## -----------------------------------------------------------------------------
rebase(chained_index, chained_index[, "202004"])

## -----------------------------------------------------------------------------
as.matrix(chained_index)

## -----------------------------------------------------------------------------
as.data.frame(chained_index)

## -----------------------------------------------------------------------------
update(pias, index) |>
  as.data.frame()

