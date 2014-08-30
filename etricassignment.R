etric.buildAEModel <- function(etric, assigment){
  if(is.null(assigment)) return(etric)
  etric <- createAEL1Constraint(etric, assigment)
  etric <- createAEL2Constraint(etric, assigment)
  etric <- createAEL3Constraint(etric, assigment)
  etric <- createAEU1Constraint(etric, assigment)
  etric <- createAEU2Constraint(etric, assigment)
  etric <- createAEU3Constraint(etric, assigment)
  return(etric)
}

createAEL1Constraint <- function(etric, assignment){
  J <- 1:etric$m
  rownames <- paste0("AEL1.",1:nrow(assignment))
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrow(assignment), dimnames=list(rownames, colnames(etric$constr$lhs)))
  for(i in 1:nrow(assignment)){
    a <- assignment[i, ]
    lhs[i, etricutils.cAB(J, a[1], a[2] - 1)] <- 1
    lhs[i, etricutils.L()] <- -1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, matrix(">=", nrow=nrow(assignment), dimnames=list(rownames)))
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(0, nrow=nrow(assignment), dimnames=list(rownames)))
  return(etric)
}

createAEL2Constraint <- function(etric, assignment){
  J <- 1:etric$m
  rownames <- paste0("AEL2.",1:nrow(assignment))
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrow(assignment), dimnames=list(rownames, colnames(etric$constr$lhs)))
  for(i in 1:nrow(assignment)){
    a <- assignment[i, ]
    lhs[i, etricutils.cBA(J, a[2] - 1, a[1])] <- 1
    lhs[i, etricutils.L()] <- -1
    lhs[i, etricutils.e()] <- 1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, matrix("<=", nrow=nrow(assignment), dimnames=list(rownames)))
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(0, nrow=nrow(assignment), dimnames=list(rownames)))
  return(etric)
}

createAEL3Constraint <- function(etric, assignment){
  J <- 1:etric$m
  rownames <- paste0("AEL3.",1:nrow(assignment))
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrow(assignment), dimnames=list(rownames, colnames(etric$constr$lhs)))
  for(i in 1:nrow(assignment)){
    a <- assignment[i, ]
    lhs[i, etricutils.cBA(J, a[2]-1, a[1])] <- -1
    lhs[i, etricutils.cAB(J, a[1], a[2])] <- 1
    lhs[i, etricutils.e()] <- -1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, matrix(">=", nrow=nrow(assignment), dimnames=list(rownames)))
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(0, nrow=nrow(assignment), dimnames=list(rownames)))
  return(etric)
}


createAEU1Constraint <- function(etric, assignment){
  J <- 1:etric$m
  rownames <- paste0("AEU1.",1:nrow(assignment))
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrow(assignment), dimnames=list(rownames, colnames(etric$constr$lhs)))
  for(i in 1:nrow(assignment)){
    a <- assignment[i, ]
    lhs[i, etricutils.cBA(J, a[3]+1, a[1])] <- 1
    lhs[i, etricutils.L()] <- -1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, matrix(">=", nrow=nrow(assignment), dimnames=list(rownames)))
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(0, nrow=nrow(assignment), dimnames=list(rownames)))
  return(etric)
}

createAEU2Constraint <- function(etric, assignment){
  J <- 1:etric$m
  rownames <- paste0("AEU2.",1:nrow(assignment))
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrow(assignment), dimnames=list(rownames, colnames(etric$constr$lhs)))
  for(i in 1:nrow(assignment)){
    a <- assignment[i, ]
    lhs[i, etricutils.cAB(J, a[1], a[3] + 1)] <- 1
    lhs[i, etricutils.e()] <- 1
    lhs[i, etricutils.L()] <- -1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, matrix("<=", nrow=nrow(assignment), dimnames=list(rownames)))
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(0, nrow=nrow(assignment), dimnames=list(rownames)))
  return(etric)
}

createAEU3Constraint <- function(etric, assignment){
  J <- 1:etric$m
  rownames <- paste0("AEU3.",1:nrow(assignment))
  lhs <- matrix(0, ncol=ncol(etric$constr$lhs), nrow=nrow(assignment), dimnames=list(rownames, colnames(etric$constr$lhs)))
  for(i in 1:nrow(assignment)){
    a <- assignment[i, ]
    lhs[i, etricutils.cAB(J, a[1], a[3] + 1)] <- -1
    lhs[i, etricutils.cBA(J, a[3], a[1])] <- 1
    lhs[i, etricutils.e()] <- -1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, matrix(">=", nrow=nrow(assignment), dimnames=list(rownames)))
  etric$constr$rhs <- rbind(etric$constr$rhs, matrix(0, nrow=nrow(assignment), dimnames=list(rownames)))
  return(etric)
}