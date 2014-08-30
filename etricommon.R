M <- 1E+10
MINEPS <- 1E-10

etriutils.solve <- function(solver, objectioveFunction, varTypes, lhs, dir, rhs, max){
  obj <- L_objective(objectioveFunction)
  roiConst <- L_constraint(lhs, dir, rhs)
  
  lp <- OP(objective=obj, constraints=roiConst, maximum=max, types=varTypes)
  
  ret <- ROI_solve(lp, solver)
  
  return(ret)
}

etriutils.getConstraintTypes <- function(allvarnames, binaryVarNames){
  types <- matrix("C", nrow = 1, ncol=length(allvarnames))
  colnames(types) <- allvarnames
  types[,binaryVarNames] <- "B"
  return(types[1,])
}

etriutils.buildObjectiveFunction <- function(varnames, objectivesVarNames){
  row <- matrix(0, ncol=length(varnames))
  colnames(row) <- varnames
  row[,objectivesVarNames] <- 1
  return(row[1,])
}