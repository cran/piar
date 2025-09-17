## -----------------------------------------------------------------------------
set.seed(54321)

library(piar)

# Make an aggregation structure.
#                  1
#      |-----------|-----------|
#      11          12          13
#  |---+---|   |---+---|   |---+---|
# 111     121 121     122 131     132

pias <- data.frame(
  level1 = rep(1, 12),
  level2 = rep(c(11, 12, 13), each = 4),
  level3 = rep(c(111, 112, 121, 122, 131, 132), each = 2),
  ea = sprintf("B%02d", 1:12),
  weight = 1:12
) |>
  as_aggregation_structure()

pias

# Make elementary indexes over 4 quarters.
elementals <- matrix(
  runif(12 * 4, 0.4, 1.2),
  nrow = 12,
  dimnames = list(sprintf("B%02d", 1:12), paste0("Q", 1:4))
) |>
  as_index()

elementals

## -----------------------------------------------------------------------------
weights(pias) / rowMeans(as.matrix(chain(elementals)))

## -----------------------------------------------------------------------------
# Function to adjust annual weights.
adjust_weights <- function(
  index,
  pias,
  tol = .Machine$double.eps^0.5,
  max_iter = 100
) {
  adj_pias <- pias
  for (i in seq_len(max_iter)) {
    # Parentally impute missing elementary indexes.
    agg_index <- aggregate(index, adj_pias, na.rm = TRUE, contrib = FALSE)
    elementals <- chain(agg_index[levels(pias)[[nlevels(pias)]]])
    # Compute annual elementary indexes.
    pb <- rowMeans(as.matrix(elementals))
    # Stop if average price-update weights are within tolerance of original
    # weights; adjust otherwise.
    if (max(abs(pb * weights(adj_pias) - weights(pias))) < tol) {
      message(gettextf("Converged after %d iterations", i - 1))
      return(adj_pias)
    } else {
      weights(adj_pias) <- weights(pias) / pb
    }
  }
  warning("weights adjustment did not converge")
  adj_pias
}

## -----------------------------------------------------------------------------
elementals[11:12] <- NA

adjust_weights(elementals, pias)

