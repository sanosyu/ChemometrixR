#　Preprocessing functions for spectrum data

library(signal)

SNV <- function(x){
  # x: data, query align to row
  Mean <- apply(x, 1, mean)
  SD <- apply(x, 1, sd)
  res <- sweep(x  , 1, Mean, FUN = '-')
  res <- sweep(res, 1, SD,   FUN = "/")
  colnames(res) <- colnames(x)
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
  colnames(res) <- colnames(x)[-Cut]
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
  inc_center <- (n -1) / 2
  inc_max    <- n - 1
  iter = length(SD) - inc_max
  
  div_pts <- sapply(1:iter, function(x){
    df1 <- x_sd[x:(x+inc_center)]
    df2 <- x_sd[(x+inc_center):(x+inc_max)]
    flag1 <- order(df1, decreasing = T) - seq(1, inc_center+1)
    flag2 <- order(df2, decreasing = F) - seq(1, inc_center+1)
    if( all(flag1 == 0) == TRUE && all(flag2 == 0) == TRUE ) { x + inc_center } else { NA }
  })
  
  div_pts <- div_pts[!is.na(div_pts)]
  SFD <- data.frame(Ini = append(x = div_pts + 1, values = 1, after = 0), Fin = append(x = div_pts, values = length(SD)))
  row.names(SFD) <- NULL
  return(SFD)
}


train_test_split <- function(x, segmentLength = 15, test_size = 0.3, random_seed = NULL){
  if(!is.null(random_seed)){ set.seed(random_seed)}
  n <- segmentLength
  if(x %% n == 0){
    test <- c()
    grs <- x/n
    test <- sapply(1:grs, function(i){
      ini <- 1+(i-1)*n
      fin <- n+(i-1)*n
      sample(seq(ini, fin), n*test_size, replace = F)
    })
    train <- seq(1, x)[-test]
    res <- c()
    res$train <- train[order(train)]
    res$test  <- test[order(test)]
    return(res)
  } else {
    print("Object number is not multiply of segment length.")
    return(NULL)
  }
}


