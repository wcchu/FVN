rm(list = ls())

library(dplyr)
library(data.table)
require(MASS)
source("dnn.R")

data <- Cars93

## random sampling 2/3 events as reference data and the rest as query
data$samp <- sample(nrow(data), nrow(data))
nref <- as.integer(nrow(data) * 0.66)
ref <-
  data %>%
  dplyr::filter(samp <= nref) %>%
  dplyr::select(-samp)
que <-
  data %>%
  dplyr::filter(samp > nref) %>%
  dplyr::select(-samp)

## Example 1: Take 2 numerical variables to predict the price
dref <- ref %>% dplyr::select(Horsepower, Passengers, Price)
dque <- que %>% dplyr::select(Horsepower, Passengers, Price)
m <- ncol(dref)
p <-
  dNN(
    x = dref[, -m],
    y = dref[, m],
    q = dque[, -m],
    v = 0.2,
    min.pts = 1,
    min.frac = 0.2
  )
dpred <-
  data.frame(dque, p) %>%
  dplyr::mutate(percentage_error = signif(abs(prediction - Price) / Price * 100, 3))
ex1 <- dpred

## Example 2: Take 3 numerical variables to predict the price
dref <- ref %>% dplyr::select(Horsepower, Passengers, MPG.city, Price)
dque <- que %>% dplyr::select(Horsepower, Passengers, MPG.city, Price)
m <- ncol(dref)
p <-
  dNN(
    x = dref[, -m],
    y = dref[, m],
    q = dque[, -m],
    v = 0.2,
    min.pts = 1,
    min.frac = 0.2
  )
dpred <-
  data.frame(dque, p) %>%
  dplyr::mutate(percentage_error = signif(abs(prediction - Price) / Price * 100, 3))
ex2 <- dpred

## Example 3: Take 4 numerical variables to predict the price
dref <- ref %>% dplyr::select(Horsepower, Passengers, MPG.city, Wheelbase, Price)
dque <- que %>% dplyr::select(Horsepower, Passengers, MPG.city, Wheelbase, Price)
m <- ncol(dref)
p <-
  dNN(
    x = dref[, -m],
    y = dref[, m],
    q = dque[, -m],
    v = 0.2,
    min.pts = 1,
    min.frac = 0.2
  )
dpred <-
  data.frame(dque, p) %>%
  dplyr::mutate(percentage_error = signif(abs(prediction - Price) / Price * 100, 3))
ex3 <- dpred

##
compare <-
  data.frame(
    que$Price,
    pred1 = ex1$prediction,
    comm1 = ex1$comment,
    nn1 = ex1$n_neighbors,
    err1 = ex1$percentage_error,
    pred2 = ex2$prediction,
    comm2 = ex2$comment,
    nn2 = ex2$n_neighbors,
    err2 = ex2$percentage_error,
    pred3 = ex3$prediction,
    comm3 = ex3$comment,
    nn3 = ex3$n_neighbors,
    err3 = ex3$percentage_error
  )

#write.csv(dref, "ref.csv")
#write.csv(dque, "que.csv")