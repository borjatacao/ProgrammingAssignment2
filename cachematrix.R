## Since Matrix inversion is usually a costly computation,
## this script contains a set of functions to cache the inverse of a matrix once it has been calculated.
## In order to do so, we create a special matrix object that stores in cache the value 
## of its inverse once it is first known.
## 
## We make the assumption that the matrix is square and invertible,
## thus we avoid checking out whether the matrix can be invertible before computing its inverse:
##   

## Function that creates a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
      x <<- y
      xinv <<- NULL
    }
    get <- function() x
    setmean <- function(inv) xinv <<- inv
    getinverse <- function() xinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function that computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then this funciton retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinverse()
    if(!is.null(xinv)) {
      message("getting cached data")
      return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setmean(xinv)
    xinv
}
