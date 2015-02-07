library(stringr) # for tests cases

constraintsToString <- function(lhs, dir, rhs){
  res <- matrix("", nrow=nrow(lhs), ncol=1, dimnames=list(rownames(lhs)))
  for(j in 1:nrow(lhs)){
    for(i in 1:ncol(lhs)){
      if(lhs[j,i] != 0){
        if(lhs[j,i] > 0){
          sign <- "+"
          if(res[j,] == "") {
            sign <- "" 
          }
          if(lhs[j,i] == 1){
            res[j,] <- paste(res[j,], sign ,colnames(lhs)[i])
          }else{
            res[j,] <- paste(res[j,],sign,lhs[j,i],colnames(lhs)[i])
          }
        }else{
          if(lhs[j,i] == -1){
            res[j,] <- paste(res[j,],"-",colnames(lhs)[i])
          }else{
            res[j,] <- paste(res[j,],lhs[j,i],colnames(lhs)[i])
          }
        }
      }
    }
    res[j,] <- paste(res[j,],dir[j,],rhs[j,])
  }
  return(res)
}
