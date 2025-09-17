## -----------------------------------------------------------------------------
library(piar)

elementals <- ms_prices |>
  transform(
    imputed_price = carry_forward(price, period = period, product = product)
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

