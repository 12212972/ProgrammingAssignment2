
## Put comments here that give an overall description of what your
## functions do

## The "MakeCacheMatrix()" function saves a pair of input matrix and its inverse as a list named "cache".

# The first one  saves a input matrix and its inverse pair in a list,
# then second one returns the inverse matrix of the input by calculation 
# or by calling from the cached data if possible.

## Write a short comment describing this function

# input and its inverse matrix data is saved in a list named "cache".
 


makeCacheMatrix <- function(x = matrix()) {
    cache <<- list(x=x, y=solve(x))
}






## Write a short comment describing this function

# This function serches for the input matrix within cached inverse matrix data(cache$y).
# The function returns its pair inverse matrix when the search succeeds.
# The inverse is newly calculated if the search fails.



###########################

cacheSolve <- function(x, ...)  {
    ## Return a matrix that is the inverse of 'x'
   
    if (!is.null(cache$y)){
        for (i in length(cache$y)){
            if (is.na(x[i]) == TRUE || x[i] - cache$y[i] != 0) {
                break
            }else{
                print("getting cached data...")
                return(cache$x)
            }
        
        }
    } 

        print("computing...")
        return(solve(x))    
    
}
  



