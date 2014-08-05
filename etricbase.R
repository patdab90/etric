etric.baseModel <- function(etric){
  
}

createB1Constraint <- function(etric){
  lhs <- matrix(0, ncol=ncol(etrib$constr$lhs), nrow=1, dimnames=list("B1",colnames(etric$constr$lhs)))
  weigthConst[, etricutils.w(1:etric$m)] <- 1
  etric$constr$lhs <- rbind(etrib$constr$lhs, weigthConst)
  
  etric$constr$rhs <- rbind(etrib$constr$rhs, matrix(1, ncol=1, nrow=1, dimnames=list("B1")))
  etric$constr$dir <- rbind(etrib$constr$dir, matrix("==", ncol=1, nrow=1, dimnames=list("B1")))
  return(etric)
}

createB2Constraint <- function(etric){
  for(j in 1:etric$m){
    wName <- etricutils.w(j)
    lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(paste0("B2.",j), colnames(etric$constr$lhs)))
    lhs[,wName] <- 1
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    
    #tymczasowo wszytskie wagi są równe
    etric$constr$rhs <- rbind(etric$constr$rhs, matrix(1/etric$m, ncol=1, nrow=1, dimnames=list(paste0("B2.",j))))
    etric$constr$dir <- rbind(etric$constr$dir, matrix("==", ncol=1, nrow=1, dimnames=list(paste0("B2.",j))))
  }
  return(etric)
}

outranking <- function(x, y, qb, qa, pb, pa, ascending){
  px <- pa * x + pb
  py <- pa * y + pb
  qx <- qa * x + qb
  qy <- qa * y + qb
  diff <- y - x
  if(ascending == FALSE){
    diff <- 0 - diff
  }
  if(diff >= px){
    return(0)
  }else if(diff <= qx){
    return(1)
  }else 
    return((px - diff)/(px - qx))
}