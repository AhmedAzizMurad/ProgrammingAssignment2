## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  setf <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  getf <- function() x
  Invrsset <- function(invrt) invrs <<- invrt
  Invrsget <- function() invrs
  list(setf = setf,
       getf = getf,
       Invrsset = Invrsset,
       Invrsget = Invrsget)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  invrs <- x$Invrsget()
  
  if (!is.null(invrs)) 
    {
    
    message("getting cached data")
    
    return(invrs)
  }
  
  matrx <- x$getf()
  invrs <- solve(matrx, ...)
  x$Invrsset(invrs)
  invrs
  
}
