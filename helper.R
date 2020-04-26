#

SNV <- function(x){
  # x: data, query align to row
  
  Mean <- apply(x, 1, mean)
  SD <- apply(x, 1, sd)
  res <- x
  res <- sweep(res, 1, Mean, FUN = '-')
  res <- sweep(res, 1, SD,   FUN = "/")
  return(res)
}