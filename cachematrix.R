## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix which is really a list containing functions to:
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL 
      set <- function(y) { ## sets x = y
            x <<- y
            inv <<- NULL
      }
      get <- function() x ## returns x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Checks to see if matrix inverse has been calculated. 
## If no, calculates inverse and stores that so it doesn't 
## need to be re-calculated.
## If yes, no calculation needed, return inverse.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      
      if (!is.null(inv)) { ## the inverse has already been calculated
            message("getting cached data")
            return(inv)
      }
      
      mat <- x$get() ## the matrix to be inverted
      inv = solve(mat)
      x$setInverse(inv)
      
      ## Return a matrix that is the inverse of 'x'
      return (inv)
}
