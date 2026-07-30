library(piar)

p2 <- c(1.2, 3, 1.3, 0.7, 1.4, 0.8)
p1 <- 1
q2 <- c(0.8, 0.9, 1.9, 1.3, 4.7, 0.6)
q1 <- c(1, 1, 2, 1, 4.5, 0.5)

rel <- p2 / p1

s1 <- scale_weights(p1 * q1)
s2 <- scale_weights(p2 * q2)

v <- transmute_weights(rel, s1, order = 2)

all.equal(gmean(rel, s1, order = 2), gmean(rel, v))

(v - s1)[order(rel)]

all.equal(
  transmute_weights(rel, v, order = 1, to = 0),
  transmute_weights(rel, s1, order = 2, to = 0)
)

v1 <- transmute_weights2(rel, list(s1, s2))

all.equal(nested_gmean(rel, list(s1, s2)), gmean(rel, v1))

all.equal(
  v1,
  transmute_weights(
    rel,
    transmute_weights2(rel, list(s1, s2), to = 2),
    order = 2
  )
)

v2 <- transmute_weights2(rel, list(s1, s2), pivot = 1)

all.equal(nested_gmean(rel, list(s1, s2)), gmean(rel, v2))

all.equal(
  v2,
  transmute_weights(
    rel,
    transmute_weights2(rel, list(s1, s2), to = 2, pivot = 1),
    order = 2
  )
)

summary(v1 - v2)

group <- rep(c("a", "b"), each = 3)

s1_by_group <- split(s1, group)
rel_by_group <- split(rel, group)

index_a <- gmean(rel_by_group$a, s1_by_group$a, order = 2)
index_b <- gmean(rel_by_group$b, order = 0)

gmean(c(index_a, index_b), sapply(s1_by_group, sum), order = 2)

decomp_a <- transmute_weights(rel_by_group$a, s1_by_group$a, order = 2)
decomp_b <- transmute_weights(rel_by_group$b, order = 0)

v <- Map(
  `*`,
  transmute_weights(c(index_a, index_b), sapply(s1_by_group, sum), order = 2),
  list(decomp_a, decomp_b)
) |>
  unlist()

gmean(rel, v)

V <- sum(p2 * q2) / sum(p1 * q1)

v <- transmute_weights2(rel, list(s1, s2), to = -1)

all.equal(
  gmean(V / rel, v),
  nested_gmean(q2 / q1, list(s1, s2))
)

V / rel * v

contraharmonic_mean <- function(x, weights, order) {
  gmean(x, weights * x)
}

# Arithmetic hybrid index
all.equal(
  gmean(p2 / p1, p2 * q1),
  contraharmonic_mean(p2 / p1, p1 * q1)
)

# Palgrave index
all.equal(
  gmean(p2 / p1, p2 * q2),
  contraharmonic_mean(p2 / p1, p1 * q2)
)

