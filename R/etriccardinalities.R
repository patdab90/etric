etric.buildCCModel <- function(etric, card){
  etric <- createCC1Constraint(etric)
  etric <- createCC21Constraint(etric)
  etric <- createCC22Constraint(etric)
  etric <- createCC23Constraint(etric)
  etric <- createCC24Constraint(etric)
  etric <- createCC31Constraint(etric)
  etric <- createCC32Constraint(etric)
  etric <- createCC33Constraint(etric)
  etric <- createCC34Constraint(etric)
  if(!is.null(card)){
    
    etric <- createCC4Constraint(etric, card)
    etric <- createCC5Constraint(etric, card)
  }
  return(etric)
}

createCC1Constraint <- function(etric){
  nrows <- etric$n
  rownames <- paste0('CC1.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(1, nrow=nrows, dimnames=list(rownames))
  for(a in 1:nrows){
    lhs[a,etricutils.vAH(a, 1:etric$p)] <- 1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}


createCC21Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC21.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH1(a, h)] <- M
      lhs[r,etricutils.cAB(J, a, h)] <- 1
      lhs[r,etricutils.e()] <- 1
      lhs[r,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}



createCC22Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC22.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(-M, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH2(a, h)] <- -M
      lhs[r,etricutils.cBA(J, h, a)] <- 1
      lhs[r,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}


createCC23Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC23.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH3(a, h)] <- M
      lhs[r,etricutils.cAB(J, a, h+1)] <- 1
      lhs[r,etricutils.cBA(J, h, a)] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC24Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC24.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(0, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH1(a, h)] <- 1
      lhs[r,etricutils.vAH2(a, h)] <- 1
      lhs[r,etricutils.vAH3(a, h)] <- 1
      lhs[r,etricutils.vAH(a, h)] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

###----------


createCC31Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC31.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH4(a, h)] <- M
      lhs[r,etricutils.cBA(J, h, a)] <- 1
      lhs[r,etricutils.e()] <- 1
      lhs[r,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}



createCC32Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC32.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(-M, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH5(a, h)] <- -M
      lhs[r,etricutils.cAB(J, a, h)] <- 1
      lhs[r,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}


createCC33Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC33.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH6(a, h)] <- M
      lhs[r,etricutils.cBA(J, h-1, a)] <- 1
      lhs[r,etricutils.cAB(J, a, h)] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC34Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC34.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(0, nrow=nrows, dimnames=list(rownames))
  r <- 0
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      r <- r + 1
      lhs[r,etricutils.vAH4(a, h)] <- 1
      lhs[r,etricutils.vAH5(a, h)] <- 1
      lhs[r,etricutils.vAH6(a, h)] <- 1
      lhs[r,etricutils.vAH(a, h)] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC4Constraint <- function(etric, card){
  specifiedCard <- card[card[,2] > -1,]
  if(!is.matrix(specifiedCard)){
    specifiedCard <- matrix(specifiedCard, ncol=ncol(cardinalities),byrow=TRUE)
  }
  nrows <- nrow(specifiedCard)
  if(nrows == 0){
    return(etric)
  }
  nrows <- nrow(specifiedCard)
  rownames <- paste0('CC4.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(specifiedCard[,2], nrow=nrows, dimnames=list(rownames))
  for(h in 1:nrows){
    lhs[h,etricutils.vAH(1:etric$n, specifiedCard[h,1])] <- 1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC5Constraint <- function(etric, card){
  specifiedCard <- card[card[,3] > -1,]
  if(!is.matrix(specifiedCard)){
    specifiedCard <- matrix(specifiedCard, ncol=ncol(cardinalities),byrow=TRUE)
  }
  nrows <- nrow(specifiedCard)
  if(nrows == 0){
    return(etric)
  }
  rownames <- paste0('CC5.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(specifiedCard[,3], nrow=nrows, dimnames=list(rownames))
  for(h in 1:nrows){
    lhs[h,etricutils.vAH(1:etric$n, specifiedCard[h,1])] <- 1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}