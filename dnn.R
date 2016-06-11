library(dplyr)
library(data.table)
# Input:
#   x = predictor variables of the reference dataset
#   y = response of the reference dataset
#   q = predictor variables of the query dataset
#   d = normalized "effective radius" of the neighborhood in the variable space;
#       actual radius = sqrt(num_of_variables * d ^ 2)
#   min.pop = minimum total population in the neighborhood
#   min.frac = minimum proportion of the most popular class out of the total
#              population to quarantee that class as the prediction
#              (only used when response is categorical)
# Output: predicted response vector
dNN <- function(x, y, q, d, min.pop = 1, min.frac = 0) {
  # * process training data to create "reference data" for NN alg
  res_type <- ifelse(is.numeric(y), 'numeric', 'class')
  res_data_type <- ifelse(is.numeric(y), 'numeric', 'character')
  ref <- data.frame(x, dnn.response = y)
  nvar <- ncol(x) # number of variables
  nref <- nrow(ref) # number of reference points
  # calculate the unit length of each variable
  u <- c()
  for (j in 1:nvar) {
    u[j] <- sd(x[, j])
  }
  # run through queries and predict response for each query
  nque <- nrow(q)
  prediction <- vector(mode = res_data_type, length = nque)
  comment <- vector(mode = "character", length = nque)
  n_neighbors <- vector(mode = "integer", length = nque)
  for (i in 1:nque) {
    ss <- vector(mode = "numeric", length = nref)
    for (j in 1:nvar) {
      ss <- ss + ((x[, j] - as.numeric(q[i, j])) / u[j])^2
    }
    distance <- sqrt(ss)
    ref$dist <- distance
    # collect data points in ball
    dball <-
      ref %>%
      dplyr::filter(dist <= sqrt(nvar * d * d))
    n = nrow(dball)
    if (n < min.pop) {
      prediction[i] <- NA
      comment[i] <- "insufficient data points"
    } else {
      # do statistics in ball
      if (res_type == 'class') {
        sball <-
          dball %>%
          dplyr::group_by(dnn.response) %>%
          dplyr::summarise(nc = n(), frac = nc/n) %>%
          dplyr::filter(frac >= min.frac) %>%
          dplyr::arrange(desc(frac)) %>%
          data.frame()
        if (nrow(sball) == 0) {
          prediction[i] <- NA
          comment[i] <- "fraction of best class too low"
        } else {
          prediction[i] <- sball$dnn.response[1]
          comment[i] <- "categorical response predicted"
        }
      } else if (res_type == 'numeric') {
        prediction[i] <- mean(dball$dnn.response, na.rm = T)
        comment[i] <- "numeric response predicted"
      }
    }
    n_neighbors[i] <- n
  }
  return(data.frame(prediction, comment, n_neighbors))
}
