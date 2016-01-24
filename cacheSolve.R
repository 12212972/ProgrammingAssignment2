cacheSolve <- function(y) {
  
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
