## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
elementals["B4", 1:3] <- 1

elementals

