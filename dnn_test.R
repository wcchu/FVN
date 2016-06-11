rm(list = ls())

library(dplyr)
require(MASS)

## Take 4 representitive numerical variables to predict the price
data <-
  Cars93 %>%
  dplyr::select(
    MPG.city,
    Horsepower,
    Length,
    Passengers,
    Price
  )

## random sampling 2/3 events as reference data and the rest as query
data$samp <- sample(nrow(data), nrow(data))
nref <- as.integer(nrow(data) * 0.66)
dref <-
  data %>%
  dplyr::filter(samp <= nref) %>%
  dplyr::select(-samp)
dque <-
  data %>%
  dplyr::filter(samp > nref) %>%
  dplyr::select(-samp)

source("dnn.R")

p <-
  dNN(
    x = dref[, c(1, 2, 3, 4)],
    y = dref[, 5],
    q = dque[, c(1, 2, 3, 4)],
    v = 0.3,
    min.pts = 3,
    min.frac = 0.2
  )
  