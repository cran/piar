## -----------------------------------------------------------------------------
library(piar)

elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)

## -----------------------------------------------------------------------------
ms_weights$stratum <- c("TS", "TA", "TS", "TS", "TS")

ms_weights

## -----------------------------------------------------------------------------
classification_sps <- paste0(ms_weights$classification, ms_weights$stratum) |>
  expand_classification(width = c(1, 1, 2))

pias_sps <- aggregation_structure(
  c(classification_sps, list(ms_weights$business)),
  ms_weights$weight
)

pias_sps

## -----------------------------------------------------------------------------
index_sps <- aggregate(elementals, pias_sps, na.rm = TRUE)

index_sps

## -----------------------------------------------------------------------------
interacted_hierarchy <- interact_classifications(
  expand_classification(ms_weights$classification),
  expand_classification(ms_weights$stratum)
)

pias_sps2 <- lapply(
  interacted_hierarchy,
  \(x) aggregation_structure(c(x, list(ms_weights$business)), ms_weights$weight)
)

index_sps2 <- lapply(pias_sps2, \(x) {
  aggregate(index_sps, x, include_ea = FALSE)
})

## -----------------------------------------------------------------------------
Reduce(merge, index_sps2)

