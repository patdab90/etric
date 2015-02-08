getCriteriaThreshold <- function(tree, criteriaIDs){
  # if an mcdaConcept has been specified, search according to this attribute
  specification = ""
  criteriaIDs <- critIDs[[1]]
  criteria <- getNodeSet(treeCriteria, paste0("//criteria",specification))
  # create the empty output list
  out<-matrix(0,nrow = length(criteriaIDs),ncol = 7, dimnames = list(criteriaIDs, c("preferenceDirection","ind_a","ind_b", "pref_a","pref_b", "veto_a", "veto_b")))
  err <-c()
  if (length(criteria)>0){
    criteria
    for (i in 1:length(criteria)){
      elements <- getNodeSet(criteria[[i]], "criterion")
      if (length(elements)>0){
        for (j in 1:length(elements)){
          # filter for inactive criteria
          act<-getNodeSet(elements[[j]], "active")
          x <- FALSE
          if (length(act)==0){
            x <- TRUE
          }else {
            if (xmlValue(act[[1]])=="true"){
              # an active tag found which is true
              x <- TRUE
            }
          }
          if(x){
            # no actvie tag found, therefore supposed to be active
            cIDs<-xmlGetAttr(elements[[j]], "id")
            if(is.element(cIDs, criteriaIDs)){
              preferenceDirection <- getNodeSet(elements[[j]], "scale/quantitative")
              out[cIDs, "preferenceDirection"] <- XML::xmlValue(preferenceDirection[[1]])
              
              ind <-getNodeSet(elements[[j]], "thresholds/threshold[@id='ind']/linear/slope")
              ind_a <- XML::xmlValue(ind[[1]])
              out[cIDs, "ind_a"] <- sapply(parse(text=ind_a), eval) 
              
              ind <-getNodeSet(elements[[j]], "thresholds/threshold[@id='ind']/linear/intercept")
              ind_b <- XML::xmlValue(ind[[1]])
              out[cIDs, "ind_b"]  <- sapply(parse(text=ind_b), eval) 
              
              pref <-getNodeSet(elements[[j]], "thresholds/threshold[@id='pref']/linear/slope")
              pref_a <- XML::xmlValue(pref[[1]])
              out[cIDs, "pref_a"]  <- sapply(parse(text=pref_a), eval) 
              
              pref <-getNodeSet(elements[[j]], "thresholds/threshold[@id='pref']/linear/intercept")
              pref_b <- XML::xmlValue(pref[[1]])
              out[cIDs, "pref_b"]  <- sapply(parse(text=pref_b), eval) 
              
              veto <-getNodeSet(elements[[j]], "thresholds/threshold[@id='veto']/linear/slope")
              veto_a <- XML::xmlValue(veto[[1]])
              out[cIDs, "veto_a"]  <- sapply(parse(text=veto_a), eval) 
              
              veto <-getNodeSet(elements[[j]], "thresholds/threshold[@id='veto']/linear/intercept")
              veto_b <- XML::xmlValue(veto[[1]])
              out[cIDs, "veto_b"]  <- sapply(parse(text=veto_b), eval) 
            }
          }
        }
      }
    }
  }
  return(out)
}
