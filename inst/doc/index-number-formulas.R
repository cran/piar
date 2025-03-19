## -----------------------------------------------------------------------------
library(piar)
library(gpindex)

prices <- data.frame(
  period = rep(1:3, each = 6),
  product = paste0("P", 1:6),
  business = rep(c("B1", "B2"), each = 3),
  price = 1:18,
  quantity = 18:1
)

prices[c("back_price", "back_quantity")] <- 
  prices[back_period(prices$period, prices$product), c("price", "quantity")]

head(prices)

## -----------------------------------------------------------------------------
prices |>
  elemental_index(price / back_price ~ period + business, r = 1)

## -----------------------------------------------------------------------------
prices |>
  elemental_index(price / back_price ~ period + business, r = -1)

## -----------------------------------------------------------------------------
prices |>
  elemental_index(
    price / back_price ~ period + business,
    weights = back_price * back_quantity
  )

## -----------------------------------------------------------------------------
tw <- grouped(index_weights("Tornqvist"))

prices |>
  elemental_index(
    price / back_price ~ period + business,
    weights = tw(
      price, back_price, quantity, back_quantity,
      group = interaction(period, business)
    )
  )

## -----------------------------------------------------------------------------
fw <- grouped(nested_transmute(0, c(1, -1), 0))

prices |>
  elemental_index(
    price / back_price ~ period + business,
    weights = fw(
      price / back_price, back_price * back_quantity, price * quantity,
      group = interaction(period, business)
    )
  )

## -----------------------------------------------------------------------------
fisher_index <- prices |>
  elemental_index(
    price / back_price ~ period + business,
    weights = fw(
      price / back_price, back_price * back_quantity, price * quantity,
      group = interaction(period, business)
    ),
    contrib = TRUE
  )

contrib(fisher_index, "B1")

## -----------------------------------------------------------------------------
diewert_contributions <- function(p1, p0, q1, q0) {
  Pf <- fisher_index(p1, p0, q1, q0)
  Pl <- laspeyres_index(p1, p0, q0)
  wl <- scale_weights(index_weights("Laspeyres")(p0, q0))
  wp <- scale_weights(index_weights("HybridPaasche")(p0, q1))
  
  (1 / (1 + Pf) * wl + Pl / (1 + Pf) * wp) * (p1 / p0 - 1)
}

## -----------------------------------------------------------------------------
contrib(fisher_index, "B1") <- subset(prices, business == "B1") |>
  split(~period) |>
  sapply(
    \(df) diewert_contributions(
      df$price, df$back_price, df$quantity, df$back_quantity
    )
  )

contrib(fisher_index, "B1")

