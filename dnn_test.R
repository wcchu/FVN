rm(list = ls())

library(dplyr)
require(MASS)

data_raw <- Cars93

## Take 4 representitive numerical variables to predict the price
data <-
  data_raw %>%
  dplyr::select(
    MPG.city,
    Horsepower,
    Length,
    Passengers,
    Weight,
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

dpred <- data.frame(dque, p)
dpred$percentage_error <-
  round(abs(dpred$prediction - dpred$Price) / dpred$Price * 100, 2)

write.csv(dref, "ref.csv")
write.csv(dque, "que.csv")