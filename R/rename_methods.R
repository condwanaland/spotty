renamer <- function(dat, from, to){
  names(dat)[names(dat) == from] <- to
  return(dat)
}