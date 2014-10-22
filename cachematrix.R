## Put comments here that give an overall description of what your
## functions do

## this is the matix inverse cache meta function. "x" is the matrix,
## "inv" is its inverse.  Its job is to cache "x" and "inv" and to create
## callable functions that will access and set these cached values
## whenever "x" is changed, the inverse is set to NULL.  That is the
## signal that we don not currently possess cached data.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function (paramInv) inv <<- paramInv
    getInverse <- function() inv
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## the parameter "x" is the list that came from "makeCacheMatrix"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
