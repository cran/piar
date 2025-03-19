## -----------------------------------------------------------------------------
library(piar)

# Make an aggregation structure.
ms_weights[c("level1", "level2")] <- 
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

# Make a fixed-base index.
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product),
    business = factor(business, levels = ms_weights$business)
  ) |>
  elemental_index(relative ~ period + business, na.rm = TRUE)

index <- elementals |>
  aggregate(pias, na.rm = TRUE) |>
  chain()

index

## -----------------------------------------------------------------------------
pias_matrix <- as.matrix(pias)

pias_matrix

## -----------------------------------------------------------------------------
pias_matrix %*% as.matrix(index[levels(pias)$business])

## -----------------------------------------------------------------------------
pias_matrix <- as.matrix(pias) > 0 

pias_matrix %*% is.na(elementals) / rowSums(pias_matrix)

## -----------------------------------------------------------------------------
as.matrix(pias, sparse = TRUE)

