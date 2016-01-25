## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
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




## Write a short comment describing this function


cacheSolve <- function(y, ...) {
  
  for (i in 1:as.integer(length(cachematrix))){
    val <- cachematrix[[i]]$set
    
    if (all(dim(val) != dim(y))) {
        next
    }
    
    else if (all(as.vector(val) == as.vector(y))) {
        # matrix identity ch
        message("getting cached data")
        ans <- cachematrix[[i]]$get
        return(ans)
        break
        
    }else{
        next
    }
  }
  ans <- solve(y)
  return(ans)

}
