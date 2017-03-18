suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(require(MASS))

set.seed(1)
data <- data.table(Cars93)

samp <- sample(nrow(data), nrow(data)/3)
ref <- data[!samp]
que <- data[samp]

dref <- ref[, c("Horsepower", "Passengers", "Price")]
dque <- que[, c("Horsepower", "Passengers", "Price")]

suppressMessages(source("fvn.R"))
p <-
  fvn(
    x = dref[, c("Horsepower", "Passengers")],
    y = dref[["Price"]],
    q = dque[, c("Horsepower", "Passengers")],
    v = 0.2,
    min.pts = 1,
    min.frac = 0.2,
    k.fnn = 10)