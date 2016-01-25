## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function calculates inverse matrix of the input 
## and put the result into a list named "cachematrix".
## The "cachematrix" can save up to 11 inverse matrices.

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

## This function compares input matrix with the one in cache.
## If the same matrix is found, it returns corresponding cached inverse matrix,
## otherwise, calculation with solve() is conducted.

cacheSolve <- function(y, ...) {
  
  for (i in 1:as.integer(length(cachematrix))){
    val <- cachematrix[[i]]$set
    
    if (all(dim(val) != dim(y))) {
        next
    }
    
    else if (all(as.vector(val) == as.vector(y))) {          # check if input matrix is the same as the one in the cache
        
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
