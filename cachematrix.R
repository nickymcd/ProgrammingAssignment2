##Matrix inversion allows the caching of the inverse of a matrix.
##This avoids the need to compute it repeatedly.
##The following two functions are used to cache the inverse of a matrix.

## The makeCacheMatrix functions creates a special 'matrix'that:
##1. sets the value of the matrix
##2. gets the value of the matrix
##3. sets the value of the inverse of the matrix
##4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list
        (set = set, 
         get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
  }



## The cacheSolve function returns the inverse of the special 'matrix' created with
## the MakeCacheMatrix function. It checks if the inverse has already been calculated
## If so, it gets the result and does not calculate it. If not, it calculates the
## inverse of the matrix and sets the value of the inverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
  
