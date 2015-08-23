## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # store the cached inverse matrix
  invMatrix <- NULL
  
  # Set matrix
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  # Get matrix
  get <- function() x
  
  # Set inverse
  setinv <- function(inverse) invMatrix <<- inverse
  # Get inverse
  getinv <- function() invMatrix
  
  # Return
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return inverse matrix of 'x'
        invMatrix <- x$getinv()
        
        ## if inverse is cached return it
        
        if (!is.null(invMatrix)) {
          message("getting cached inverse matrix")
          return(invMatrix)
        }
        
        ## else calculate inverse
        data <- x$get()
        invMatrix <- solve(data, ...)
        
        # Cache the inverse
        x$setinv(invMatrix)
        
        # Return it
        invMatrix
}
