suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(require(MASS))

data <- Cars93

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

dref <- ref %>% dplyr::select(Horsepower, Passengers, Price)
dque <- que %>% dplyr::select(Horsepower, Passengers, Price)
m <- ncol(dref)

suppressMessages(source("fvn.R"))
p <-
  fvn(
    x = dref[, -m],
    y = dref[, m],
    q = dque[, -m],
    v = 0.2,
    min.pts = 1,
    min.frac = 0.2,
    k.fnn = 10)