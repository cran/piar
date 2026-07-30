library(piar)

elementals <- ms_prices |>
  transform(
    imputed_price = impute_prices(
      price,
      period = period,
      product = product,
      method = "carry-forward"
    )
  ) |>
  elementary_index(
    price_relative(imputed_price, period = period, product = product) ~
      period + business,
    na.rm = TRUE
  )

elementals

elementals2 <- elementals
elementals2["B4", 1:3] <- 1

elementals2

ms_weights[c("level1", "level2")] <-
  expand_classification(ms_weights$classification)

pias <- ms_weights[c("level1", "level2", "business", "weight")] |>
  as_aggregation_structure()

aggregate(elementals2, pias, na.rm = TRUE)

impute <- function(x, pias) {
  if (is.na(x["B4"])) x["B4"] <- 1
  x
}

aggregate(elementals, pias, na.rm = TRUE, impute_rules = impute)

