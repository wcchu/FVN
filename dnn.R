# Input:
#   x = predictor variables of the reference dataset
#   y = response of the reference dataset
#   q = predictor variables of the query dataset
#   r = normalized radius of the neighborhood in the variable space
# Output: predicted response vector
dNN <- function(x, y, q, d, min.pop = 1, min.frac = 0) {
  # * process training data to create "reference data" for NN alg
  res_type <- ifelse(is.numeric(y), 'numeric', 'class')
  ref <- data.frame(x, dnn.response = y)
  nv <- ncol(x) # number of variables
  nr <- nrow(ref) # number of reference points
  # calculate the unit length of each variable
  u <- c()
  for (j in 1:nv) {
    u[j] <- sd(x[, j])
  }
  # run through queries and predict response for each query
  nq <- nrow(q)
  p <- c()
  for (i in 1:nq) {
    ss <- c()
    for (j in 1:nv) {
      ss <- ss + ((x[, j] - as.numeric(q[i, j])) / u[j])^2
    }
    distance <- sqrt(ss)
    y$dist <- distance
    # collect data points in ball
    dball <-
      y %>%
      filter(dist <= d)
    n = nrow(dball)
    if (n < min.pop) {
      pred <- NA
      status <- "insufficient data points"
    } else {
      # do statistics in ball
      if (res_type == 'class') {
        sball <-
          dball %>%
          group_by(y) %>%
          summarise(nc = n(), frac = nc/n) %>%
          filter(frac >= min.frac) %>%
          arrange(desc(frac)) %>%
          data.frame()
        if (nrow(sball) == 0) {
          pred <- NA
          status <- "fraction of best class too low"
        } else {
          pred <- sball$y[1]
          status <- "response predicted: class"
        }
      } else if (res_type == 'numeric') {
        pred <- mean
        status <- "response predicted: numeric"
      }
    }
    p[i] <- pred
  }
  return(p)
}
