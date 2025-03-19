## -----------------------------------------------------------------------------
library(piar)

set.seed(12345)

# Make indexes for 6 basic headings for 4 countries.
bh_index <- matrix(
  c(rep(1, 6), runif(6 * 3, 0.8, 1.2)),
  nrow = 6,
  dimnames = list(
    paste0("BH", 1:6),
    paste("Country", 1:4)
  )
) |>
  as_index(chainable = FALSE)

head(bh_index)

# Make fixed aggregation weights.
#            1
#      |-----+-----|
#      11          12
#  |---+---|   |---+---|
#  B1  B2  B3  B4  B5  B6

weights <- data.frame(
  level1 = 1,
  level2 = rep(11:12, each = 3),
  bh = levels(bh_index),
  weights = runif(6, 100, 200)
)

head(weights)

## -----------------------------------------------------------------------------
index <- aggregate(bh_index, weights)

## -----------------------------------------------------------------------------
as.matrix(index[1, -1])

update_factors <- runif(3, 0.8, 1.2)

as.matrix(index[1, -1]) * update_factors

