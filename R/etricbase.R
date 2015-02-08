etric.buildBaseModel <- function(etric, performances, profiles, th, monotonicity){
  etric <- createB1Constraint(etric)
  etric <- createB2Constraint(etric)
  etric <- createB3Constraint(etric)
  etric <- createB4Constraint(etric)
  etric <- createB5Constraint(etric)
  etric <- createB6Constraint(etric, performances, profiles, th, monotonicity)
  return(etric)
}

createB1Constraint <- function(etric){
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=1, dimnames=list("B1",colnames(etric$constr$lhs)))
  lhs[, etricutils.w(1:etric$m)] <- 1
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(1, ncol=1, nrow=1, dimnames=list("B1")))
  etric$constr$dir <- rbind(etric$constr$dir, matrix("==", ncol=1, nrow=1, dimnames=list("B1")))
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

createB3Constraint <- function(etric){
  J <- 1:etric$m
  nrow <- (etric$p-1)
  H <- 1:nrow
  rownames <- paste0("B3.",1:nrow)
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrow, dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrow, dimnames=list(rownames))
  rhs <- matrix(0, nrow=nrow, dimnames=list(rownames))
  for(h in H){
    lhs[h, etricutils.cBB(J, h)] <- 1
    lhs[h, etricutils.e()] <- 1
    lhs[h, etricutils.L()] <- -1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

createB4Constraint <- function(etric){
  rownames <- paste0("B4.",1:2)
  
  lhs <- matrix(0, nrow=2, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  lhs[,"L"] <- 1
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(c(1, 0.5), ncol=1, nrow=2, dimnames=list(rownames)))
  etric$constr$dir <- rbind(etric$constr$dir, matrix(c("<=",">="), ncol=1, nrow=2, dimnames=list(rownames)))
  
  return(etric)
}

createB5Constraint <- function(etric){
  W <- paste0("w",1:etric$m)
  m <- length(W)
  rownames <- paste0("B5.",1:(m*2))
  lhs <- matrix(0, nrow = m*2, ncol=ncol(etric$constr$lhs),
                dimnames = list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(rep.int(c(">=","<="),times=m), nrow = m*2, ncol = 1, dimnames = list(rownames))
  rhs <- matrix(rep.int(c(0, 1),times=m), nrow = m*2, ncol = 1, dimnames = list(rownames))
  
  row <- 0
  for(name in W){
    row <- row + 1
    lhs[row,name] <- 1
    
    row <- row + 1
    lhs[row,name] <- 1
    
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  
  return(etric)
}

createB6Constraint <- function(etric, performances, profiles, th, monotonicity){
  nrows <- etric$m*(etric$p+2)*etric$n*2+(etric$p-1)*etric$m
  
  rownames <- paste0("B6.", 1:nrows)
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrows, dimnames=list(rownames, colnames(etric$constr$lhs)))
  row <- 0
  for (j in 1:etric$m) {
    for (a in 1:etric$n) {
      for (b in 0:(etric$p+1)) {
        row = row + 1        
        indAB <- etricutils.cAB(j, a, b)
        lhs[row,indAB] = 1
        val = outranking(performances[a,j], profiles[b+1,j],
                         th[j,2], th[j,1], th[j, 4], th[j, 3], monotonicity[j])
        lhs[row, etricutils.w(j)] = -1 * val
      }
    }
  }
  for (j in 1:etric$m) {
    for (b in 0:(etric$p+1)) {
      for (a in 1:etric$n) {
        row = row + 1        
        indBA <- etricutils.cBA(j, b, a) 
        lhs[row, indBA] = 1
        val <-  outranking(profiles[b+1,j], performances[a,j],
                           th[j,2], th[j,1], th[j, 4], th[j, 3], monotonicity[j])
        lhs[row, etricutils.w(j)] = -1 * val
      }
    }
  }

  for (j in 1:etric$m) {
    for (b in 1:(etric$p-1)) {
      row = row + 1        
      indBB <- etricutils.cBB(j, b) 
      lhs[row,indBB] = 1
      val <-  outranking(profiles[b+1, j], profiles[b+2, j],
                         th[j,2], th[j,1], th[j, 4], th[j, 3], monotonicity[j])
      lhs[row, etricutils.w(j)] = -1 * val     
    }
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  
  dir <- matrix("==", nrow=row, ncol=1)
  rownames(dir) <- rownames
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  rhs <- matrix(0, nrow=nrows, ncol=1)
  rownames(rhs) <- rownames
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  
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