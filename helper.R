#　Preprocessing functions for spectrum data

library(signal)

SNV <- function(x){
  # x: data, query align to row
  Mean <- apply(x, 1, mean)
  SD <- apply(x, 1, sd)
  res <- sweep(x  , 1, Mean, FUN = '-')
  res <- sweep(res, 1, SD,   FUN = "/")
  return(res)
}


SG <- function(x, m, p, n){
  # Savitzky-Gollay fileter
  # x: data, query align to row
  # m: rank for derivative, 0: smoothing, 1: 1st derivative, 2: 2nd derivative
  # p: polynomial order for regression curve
  # n: window size, odd integar
  
  res <- apply(x, 1, sgolayfilt, m = m, p = p, n = n )
  res <- t(res)
  Edge <- (n-1)/2
  Cut <- c(1:Edge, tail(1:ncol(res), n = Edge))
  res <- res[,-Cut]
  return(res)
}


