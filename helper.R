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


PP_pipe <- function(x, step){
  # preprocessing of spectrum data
  # x: data, query align to row
  # step: vector of preprocessing and their parameters like c('smooth_p_n', 'sg_m_p_n', 'snv')
  
  res <- x
  for (i in 1:length(step)){
    type <- unlist(strsplit(step[i], split = '_'))
    if(type[1] == 'snv')    res <- SNV(res)
    if(type[1] == 'sg')     res <- SG(res, as.numeric(type[2]), as.numeric(type[3]), as.numeric(type[4]))
  }
  return(res)
}


SFD <- function(x, n){
  # spectral fluctuation dividing
  #
  # x: data, query align to row
  # n: window size
  
  SD <- apply(x, 2, sd)
  
}


