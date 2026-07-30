library(piar)

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

prices |>
  elementary_index(price / back_price ~ period + business, order = 1)

prices |>
  elementary_index(price / back_price ~ period + business, order = -1)

prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = back_price * back_quantity
  )

period_by_business <- interaction(prices$period, prices$business)

tornqvist_weights <- split(prices, ~period_by_business) |>
  lapply(\(df) {
    0.5 * scale_weights(df$price * df$quantity) +
      0.5 * scale_weights(df$back_price * df$back_quantity)
  }) |>
  unsplit(period_by_business)

prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = tornqvist_weights
  )

fisher_weights <- split(prices, ~period_by_business) |>
  lapply(\(df) {
    transmute_weights2(
      df$price / df$back_price,
      list(df$back_price * df$back_quantity, df$price * df$quantity),
      to = 0
    )
  }) |>
  unsplit(period_by_business)

prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = fisher_weights
  )

fisher_index <- prices |>
  elementary_index(
    price / back_price ~ period + business,
    weights = fisher_weights,
    contrib = TRUE
  )

contrib(fisher_index, "B1")

diewert_contributions <- function(p1, p0, q1, q0) {
  rel <- p1 / p0
  v0 <- scale_weights(p0 * q0)
  v1 <- scale_weights(p0 * q1)
  laspeyres <- gmean(rel, v0)
  fisher <- nested_gmean(rel, list(v0, v1), order = c(1, 1))

  (v0 + laspeyres * v1) / (1 + fisher) * (rel - 1)
}

contrib(fisher_index, "B1") <- subset(prices, business == "B1") |>
  split(~period) |>
  sapply(
    \(df) {
      diewert_contributions(
        df$price,
        df$back_price,
        df$quantity,
        df$back_quantity
      )
    }
  )

contrib(fisher_index, "B1")

# ---- Arithmetic mean ----
# Carli
carli <- \(p1, p0, q1, q0) gmean(p1 / p0)

# Dutot
dutot <- \(p1, p0, q1, q0) gmean(p1 / p0, p0)

# Laspeyres
laspeyres <- \(p1, p0, q1, q0) gmean(p1 / p0, p0 * q0)

# Palgrave
palgrave <- \(p1, p0, q1, q0) gmean(p1 / p0, p1 * q1)

# Walsh-1
walsh1 <- \(p1, p0, q1, q0) gmean(p1 / p0, p0 * sqrt(q0 * q1))

# Marshall-Edgeworth
marshall_edgeworth <- \(p1, p0, q1, q0) gmean(p1 / p0, p0 * (q0 * q1))

# Geay-Khamis
geary_khamis <- \(p1, p0, q1, q0) gmean(p1 / p0, p0 / (1 / q0 + 1 / q1))

# Hybrid CSWD
hybrid_cswd <- \(p1, p0, q1, q0) gmean(p1 / p0, sqrt(p0 / p1))

# Martini
martini <- \(p1, p0, q1, q0, a) gmean(p1 / p0, p0 * q0 * (q1 / p0)^a)

# ---- Geometric mean ----
# Jevons
jevons <- \(p1, p0, q1, q0) gmean(p1 / p0, order = 0)

# Geometric Laspeyres (Jöhr)
geo_laspeyres <- \(p1, p0, q1, q0) gmean(p1 / p0, p0 * q0, order = 0)

# Geometric Paasche
geo_paasche <- \(p1, p0, q1, q0) gmean(p1 / p0, p1 * q1, order = 0)

# Walsh-2
walsh2 <- \(p1, p0, q1, q0) gmean(p1 / p0, sqrt(p0 * p1 * q0 * q1), order = 0)

# Theil
theil <- \(p1, p0, q1, q0) {
  w0 <- scale_weights(p0 * q0)
  w1 <- scale_weights(p1 * q1)
  gmean(p1 / p0, ((w0 + w1) / 2 * w0 * w1)^(1 / 3), order = 0)
}

# Rao
rao <- \(p1, p0, q1, q0) {
  w0 <- scale_weights(p0 * q0)
  w1 <- scale_weights(p1 * q1)
  gmean(p1 / p0, w0 * w1 / (w0 + w1), order = 0)
}

# Sato-Vartia
sato_vartia <- function(p1, p0, q1, q0) {
  v0 <- scale_weights(p0 * q0)
  v1 <- scale_weights(p1 * q1)
  gmean(p1 / p0, emean(v0, v1), order = 0)
}

# ---- Harmonic mean ----
# Coggeshall
coggeshall <- \(p1, p0, q1, q0) gmean(p1 / p0, order = -1)

# Paasche
paasche <- \(p1, p0, q1, q0) gmean(p1 / p0, p1 * q1, order = -1)

# Harmonic Laspeyres
harm_laspeyres <- \(p1, p0, q1, q0) gmean(p1 / p0, p0 * q0, order = -1)

# ---- Generalized mean ----
# Lloyd-Moulton
lloyd_moulton <- \(p1, p0, q1, q0, sigma) {
  gmean(p1 / p0, p0 * q0, order = 1 - sigma)
}

# ---- Nested means ----
# Drobisch (Sidgwick)
drobisch <- \(p1, p0, q1, q0) {
  nested_gmean(p1 / p0, list(p0 * q0, p1 * q1), outer_order = 1)
}

# Unnamed
unnamed <- \(p1, p0, q1, q0) {
  nested_gmean(
    p1 / p0,
    list(p0 * q0, p1 * q1),
    order = c(1, 1),
    outer_order = 1
  )
}

# Törnqvist-Theil
tornqvist <- \(p1, p0, q1, q0) {
  nested_gmean(
    p1 / p0,
    list(p0 * q0, p1 * q1),
    order = c(0, 0)
  )
}

# Fisher
fisher <- \(p1, p0, q1, q0) nested_gmean(p1 / p0, list(p0 * q0, p1 * q1))

# CSWD
cswd <- \(p1, p0, q1, q0) nested_gmean(p1 / p0)

# Balk-Walsh
balk_walsh <- \(p1, p0, q1, q0) nested_gmean(p1 / p0, order = c(0.5, -0.5))

# Geometric AG mean
ag_mean <- \(p1, p0, q1, q0, elasticity) {
  nested_gmean(
    p1 / p0,
    list(p0 * q0, p0 * q0),
    order = c(0, 1),
    outer_weights = c(elasticity, 1 - elasticity)
  )
}

