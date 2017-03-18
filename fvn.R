# Distance-based nearest neighbors algorithm
# Wei-Chun Chu
# 2016-06-12
library(dplyr)
library(data.table)
library(FNN)
# Input:
#   x = predictor variables of the reference dataset
#   y = response of the reference dataset
#   q = predictor variables of the query dataset
#   v = normalized volume of the neighborhood in the variable space;
#       normalized volume = (normalized radius) ^ {number of variables}
#   min.pts = minimum number of data points in the neighborhood
#   min.frac = minimum proportion of the most popular class out of the total
#              population to quarantee that class as the prediction
#              (only used when response is categorical)
#   k.fnn = number of neighbors to pre-subset using fnn
# Output: predicted response vector
fvn <- function(x, y, q, v, min.pts = 1, min.frac = 0, k.fnn = 0) {
  x <- data.table(x)
  q <- data.table(q)
  if (is.data.frame(y)) {y <- y[, 1]}
  # * process training data to create "reference data"
  res_type <- ifelse(is.numeric(y), 'numeric', 'class')
  res_data_type <- ifelse(is.numeric(y), 'numeric', 'character')
  ref <- data.table(x, fvn.response = y)
  nvar <- ncol(x) # number of variables
  nref <- nrow(ref) # number of reference points
  r0 <- v ** (1 / nvar)
  # normalize variable space
  u <- c()
  href <- ref
  hq <- q
  for (j in 1:nvar) {
    u[j] <- sd(x[[j]]) ## the unit length
    href[[j]] <- ref[[j]]/u[j] ## normalize ref
    hq[[j]] <- q[[j]]/u[j] ## normalize query
  }
  # run through queries and predict response for each query
  nque <- nrow(q)
  prediction <- vector(mode = res_data_type, length = nque)
  comment <- vector(mode = "character", length = nque)
  n_neighbors <- vector(mode = "integer", length = nque)
  for (i in 1:nque) {
    k0 <- ifelse(k.fnn == 0, nref, k.fnn)
    fnn_res <-
      get.knnx(
        data = href[, c(1:nvar), with = FALSE],
        query = hq[, c(1:nvar), with = FALSE][i, ],
        k = k0)
    # collect data points in the neighborhood ball
    dball <- ref[fnn_res$nn.index[which(fnn_res$nn.dist <= r0)], "fvn.response"]
    n = nrow(dball)
    if (n < min.pts) {
      prediction[i] <- NA
      comment[i] <- "insuff. points"
    } else {
      # do statistics in ball
      if (res_type == 'class') {
        sball <-
          dball %>%
          dplyr::group_by(fvn.response) %>%
          dplyr::summarise(nc = n(), frac = nc/n) %>%
          dplyr::filter(frac >= min.frac) %>%
          dplyr::arrange(desc(frac)) %>%
          data.frame()
        if (nrow(sball) == 0) {
          prediction[i] <- NA
          comment[i] <- "frac of best class too low"
        } else {
          prediction[i] <- sball$fvn.response[1]
          comment[i] <- paste(
            "class frac",
            as.character(signif(sball$frac[1], 3))
          )
        }
      } else if (res_type == 'numeric') {
        prediction[i] <- signif(mean(dball$fvn.response, na.rm = T), 3)
        comment[i] <- paste(
          "std is", 
          as.character(signif(sd(dball$fvn.response, na.rm = T), 3))
        )
      }
    }
    n_neighbors[i] <- n
  }
  return(data.frame(prediction, comment, n_neighbors))
}
