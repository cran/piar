## -----------------------------------------------------------------------------
library(piar)

# Make an aggregation structure.
ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

# Make elementary index.
elementals <- ms_prices |>
  transform(
    relative = price_relative(price, period = period, product = product)
  ) |>
  elementary_index(relative ~ period + business, na.rm = TRUE)

elementals

## -----------------------------------------------------------------------------
set.seed(12345)

scanner_prices <- data.frame(
  period = rep(c("201904", time(elementals)), each = 200),
  product = 1:200,
  price = round(rlnorm(5 * 200) * 10, 1),
  quantity = round(runif(5 * 200, 100, 1000))
)

head(scanner_prices)

## -----------------------------------------------------------------------------
library(gpindex)

geks_elementals <- with(
  scanner_prices,
  fisher_geks(price, quantity, period, product, window = 3)
) |>
  splice_index() |>
  t() |>
  as_index(chainable = FALSE) |>
  set_levels("B5") |>
  rebase("202001")

geks_elementals

## -----------------------------------------------------------------------------
merge(elementals, geks_elementals) |>
  aggregate(pias, na.rm = TRUE)

