ahptopsis2n<-function(decision, criteria, minmax){
  
  
  if(! is.matrix(decision))
    stop("decision must be a matrix with values of alternatives for each criterion")
  if(ncol(decision)>15)
    stop("15 is the maximum number for criteria")
  if(ncol(decision)<3)
    stop("3 is the minimum number for criteria")
  
  
  if(! is.matrix(criteria))
    stop("criteria must be a matrix with pairwise comparisons of criteria")
  if(ncol(criteria)!=nrow(criteria))
    stop("criteria must be a square matrix")
  if(ncol(criteria)!=ncol(decision))
    stop("criteria must be the same on criteria and decision arguments")
  if(!all(diag(criteria ==1)))
    stop("criteria must be a matrix with diagonal entries equal to one")
  if(!all(criteria == 1/t(criteria)))
    stop("criteria entries must be revised")
  
  
  if(!is.character(minmax))
    stop("mimmax must be a character vector")
  if(!all(minmax=="max" | minmax=="min"))
    stop("minmax must be filled only with the characters 'min' or 'max'")
  if(length(minmax)!= ncol(decision))
    stop("mimmax must have the same length of decision argument columns")
  
  
  
  
  normcriteria <- sweep(criteria, 2, colSums(criteria), "/")
  gmean <- apply(normcriteria, 1, function(x){prod(x)^(1/length(x))})
  weights<-gmean/sum(gmean)
  maxlambda<- colSums(criteria)%*% weights
  consistencyindex<-(maxlambda-ncol(criteria))/(ncol(criteria)-1)
  randomindices<-list("3"= 0.58, "4"= 0.9 , "5"= 1.12, "6"= 1.24, "7"= 1.32, "8"= 1.41, "9"= 1.45, "10"= 1.49, "11"=1.51, "12"=1.48, "13"=1.56,"14"=1.57, "15"=1.59)
  ncriteria<- as.character(ncol(criteria))
  consistencyratio<-consistencyindex/randomindices[[ncriteria]]
  
  if (consistencyratio > 0.10)
    stop("consistency ratio is greater than 10%, you must revise the judgments on criteria matrix")
  
  
  
  
  
  normdecision1<- sweep(decision,2, apply(decision, 2, function(x) sqrt(sum(x^2))),"/")
  weidecision1<- sweep(normdecision1, 2, weights, "*")
  possolution1<-ifelse(minmax=="max", apply(weidecision1, 2, max), apply(weidecision1, 2, min))
  negsolution1<-ifelse(minmax=="max", apply(weidecision1, 2, min), apply(weidecision1, 2, max))
  
  distpos1<- apply(weidecision1, 1, function(x) sqrt(sum((possolution1-x)^2)))
  distneg1<- apply(weidecision1, 1, function(x) sqrt(sum((negsolution1-x)^2)))
  
  result1<- data.frame(distpos1, distneg1)
  result1$values<- distneg1/(distpos1+distneg1)
  result1$ranking<-rank(-result1$values)
  result1$distpos1 <- NULL
  result1$distneg1 <- NULL
  
  
  
  
  colmin<-apply(decision, 2, min)
  colmax<-apply(decision, 2, max)
  normdecision2<-sweep(sweep(decision, 2,colmin, "-"), 2, colmax-colmin, "/")
  weidecision2<- sweep(normdecision2, 2, weights, "*")
  possolution2<-ifelse(minmax=="max", apply(weidecision2, 2, max), apply(weidecision2, 2, min))
  negsolution2<-ifelse(minmax=="max", apply(weidecision2, 2, min), apply(weidecision2, 2, max))
  
  distpos2<- apply(weidecision2, 1, function(x) sqrt(sum((x-possolution2)^2)))
  distneg2<- apply(weidecision2, 1, function(x) sqrt(sum((x-negsolution2)^2)))
  
  
  result2<- data.frame(distpos2, distneg2)
  result2$values<- distneg2/(distpos2+distneg2)
  result2$ranking<-rank(-result2$values)
  result2$distpos2 <- NULL
  result2$distneg2 <- NULL
  
  
  return(list(consistencyratio, result1, result2))
  
  
}


