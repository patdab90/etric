source('etricutils.R')
source('etricommon.R')
source('etricbase.R')
source('etricassignment.R')
source('etricpaiwisecmp.R')
source('etriccardinalities.R')
source('etricreturns.R')

etric.init <- function(performances, profiles, assignments, monotonicity, th, cardinalities, pCk, pCl){
  stopifnot(ncol(performances) == ncol(profiles))
  stopifnot(is.null(assignments) || ncol(assignments) == 3)
  stopifnot((is.null(pCk)) || (ncol(pCk) == 3))
  stopifnot(is.null(pCl) || ncol(pCl) == 3)
  stopifnot(nrow(assignments) < nrow(performances))
  stopifnot(nrow(th) == ncol(performances))
  stopifnot(nrow(th) == length(monotonicity))
  
  message("--- Constructing model")
  
  n <- nrow(performances)
  p <- nrow(profiles)-2 # p - liczba klas. Klas jest o 2 mniej niż profili
  m <- ncol(performances)
  
  A <- 1:n ## an
  B <- 0:(p+1) ## profile
  H <- 1:p ## klasy 
  J <- 1:m ## j
  
  etric <- list()
  etric$n <- n
  etric$p <- p
  etric$m <- m
  
  var <- etricutils.createVarnames(n, p, J, pCk, pCl)
  
  etric$binaryvar <- var$binary
  
  etric$constr$lhs <- intiLHS(c(var$float, var$binary))
  etric$constr$dir <- initDIR()
  etric$constr$rhs <- initRHS()
  
  etric <- etric.buildBaseModel(etric, performances, profiles, th, monotonicity)
  etric <- etric.buildAEModel(etric, assignments)
  etric <- etric.buildPCModel(etric, pCk, pCl )
  etric <- etric.buildCCModel(etric, cardinalities)
  return(etric)
}


etric.possibleAssigment <- function(etric, solver){
  possibleRanking <- matrix(FALSE,ncol=etric$p, nrow=etric$n, 
                            dimnames= list(paste0("a",1:etric$n), paste0("C", 1:etric$p)))
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      etric <- createPAModel(etric, a, h)
      objFunction <- etriutils.buildObjectiveFunction(varnames=colnames(etric$constr$lhs),objectivesVarNames=etricutils.e())
      varTypes <- etriutils.getConstraintTypes(allvarnames=colnames(etric$constr$lhs),binaryVarNames=etric$binary) 
      ret <- etriutils.solve(solver=solver, objectioveFunction=objFunction,varTypes=varTypes,
                             lhs=etric$constr$lhs,dir=etric$constr$dir, rhs=etric$constr$rhs, max=TRUE)
      possibleRanking[a,h] <-  ret$status$code == 0 && ret$objval > 0
    }
  }
  return(possibleRanking)
}


etric.necessaryAssigment <- function(etric, solver){
  necessaryRanking <- matrix(FALSE,ncol=etric$p, nrow=etric$n, 
                            dimnames= list(paste0("a",1:etric$n), paste0("C", 1:etric$p)))
  for(a in 1:etric$n){
    for(h in 1:etric$p){
      
      ret1 <- list(objval=1, list(status=list(code=0)))
      ret2 <- list(objval=1, list(status=list(code=0)))
      
      objFunction <- etriutils.buildObjectiveFunction(varnames=colnames(etric$constr$lhs),objectivesVarNames=etricutils.e())
      varTypes <- etriutils.getConstraintTypes(allvarnames=colnames(etric$constr$lhs),binaryVarNames=etric$binary) 
      if(h < etric$p){
        na1 <- createNA1Model(etric, a, h)
        ret1 <- etriutils.solve(solver=solver, objectioveFunction=objFunction,varTypes=varTypes,
                            lhs=na1$constr$lhs,dir=na1$constr$dir, rhs=na1$constr$rhs, max=TRUE)
      }
      if(h >1){
        na2 <- createNA2Model(etric, a, h)
        ret2 <- etriutils.solve(solver=solver, objectioveFunction=objFunction,varTypes=varTypes,
                                lhs=na2$constr$lhs,dir=na2$constr$dir, rhs=na2$constr$rhs, max=TRUE)
      }
      necessaryRanking[a,h] <-  ret1$status$code != 0 && ret1$objval <= 0 && ret2$status$code != 0 && ret2$objval <= 0
    }
  }
  return(necessaryRanking)
}


etric.isFeasible <- function(etric, solver){
  objFunction <- etriutils.buildObjectiveFunction(varnames=colnames(etric$constr$lhs),objectivesVarNames=etricutils.e())
  varTypes <- etriutils.getConstraintTypes(allvarnames=colnames(etric$constr$lhs),binaryVarNames=etric$binary) 
  ret <- etriutils.solve(solver, objectioveFunction=objFunction,varTypes=varTypes,
                         lhs=etric$constr$lhs,dir=etric$constr$dir, rhs=etric$constr$rhs,max=TRUE)
  return(ret$status$code == 0 && ret$objval > 0)
}

etric.necessaryPreferenceRelation <- function(etric, solver){
  necessaryRelation <- matrix(TRUE,ncol=etric$n, nrow=etric$n, 
                             dimnames= list(paste0("a",1:etric$n), paste0("a", 1:etric$n)))
  for(a in 1:etric$n){
    for(b in 1:etric$n){
      for(h in 1:(etric$p-1)){
        objFunction <- etriutils.buildObjectiveFunction(varnames=colnames(etric$constr$lhs),objectivesVarNames=etricutils.e())
        varTypes <- etriutils.getConstraintTypes(allvarnames=colnames(etric$constr$lhs),binaryVarNames=etric$binary) 
        
        pl <- createPLModel(etric, a, b, h)
        ret1 <- etriutils.solve(solver, objectioveFunction=objFunction,varTypes=varTypes,
                               lhs=pl$constr$lhs,dir=pl$constr$dir, rhs=pl$constr$rhs,max=TRUE)
        pr <- createPRModel(etric, a, b, h)
        ret2 <- etriutils.solve(solver, objectioveFunction=objFunction,varTypes=varTypes,
                               lhs=pr$constr$lhs,dir=pr$constr$dir, rhs=pr$constr$rhs,max=TRUE)
        necessaryRelation[a,b] <- necessaryRelation[a,b] && ret1$status$code != 0 && ret1$objval <= 0 && ret2$status$code != 0 && ret2$objval <= 0
        if(necessaryRelation[a,b] == FALSE){
          break;
        }
      }
    }
  }
  return(necessaryRelation)
}

etric.classCardinalities <- function(etric, max, solver){
  colname <- "MAX"
  if(!max) {
    colname <- "MIN"
  }
  classCardinalities <- matrix(0,ncol = 1, nrow = etric$p, dimnames=list(paste0("C", 1:etric$p), colname))
  for(h in 1:etric$p){
    varnames <- etricutils.vAH(1:etric$n, h)
    objFunction <- etriutils.buildObjectiveFunction(varnames=colnames(etric$constr$lhs),objectivesVarNames=varnames)
    varTypes <- etriutils.getConstraintTypes(allvarnames=colnames(etric$constr$lhs),binaryVarNames=etric$binary)  
    ret <- etriutils.solve(solver, objectioveFunction=objFunction,varTypes=varTypes,
                            lhs=etric$constr$lhs,dir=etric$constr$dir, rhs=etric$constr$rhs,max=max)
    classCardinalities[h,] <- ret$objval
  }                             
                               
  return(classCardinalities)
}

etric.createBorderProfiles <- function(alternatives, profiles, monotocity){
  minp <- rep(0,ncol(alternatives))
  maxp <- rep(0,ncol(alternatives))
  for(c in 1:ncol(alternatives)){
    if(monotocity[c]){
      minp[c] <- min(alternatives[,c])
      maxp[c] <- max(alternatives[,c])
    }else{
      minp[c] <- max(alternatives[,c])
      maxp[c] <- min(alternatives[,c])
    }
  }
  return(rbind(minp,profiles,maxp))
}

