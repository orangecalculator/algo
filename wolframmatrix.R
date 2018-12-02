wolframmatrix <- function(M){
  ## Check Condition for Running Function
  stopifnot(is.matrix(M))
  r<-nrow(M)
  l<-ncol(M)
  stopifnot(r>0&l>0)
  
  ## Note that Wolfra Alpha recognize matrices by row
  cat("{")
  for(i in 1:r){
    cat("{")
    for(j in 1:l) {
      cat(M[i,j])
      if(j!=l) cat(",")
    }
    cat("}")
    if(i!=r) cat(",")
  }
  cat("}\n")
}
