makeCacheMatrix <- function(x) {
  tempcache <<- list(set = x, get = solve(x))
  n <- 10      # set the cache size
  if (as.integer(length(cachematrix)) <= n) {
    cachematrix <<- c(cachematrix, list(tempcache))
  }else{
    for (i in 1:n - 1) {
      cachematrix[i] <<- cachematrix[i + 1]
    }
    cachematrix[[n + 1]] <<- list(set = x, get = solve(x))
 }
}
