## makeCacheMatrix() creates a function list of four functions that
## access a "hidden" cache for a matrix.  get() and getInverse() acquire
## the matrix and its inverse respectively, and set() and setInverse()
## change them.  cacheSolve() actually computes the inverse but it has to use
## the functions supplied by makeCacheMatrix() to access and to persist the
## cache.

## this is the matix inverse cache meta function. "x" is the matrix,
## "inv" is its inverse.  Its job is to cache "x" and "inv" and to create
## callable functions that will access and set these cached values.
## Whenever "x" is changed, the inverse is set to NULL.  That is the
## signal that we do not currently possess cached data.

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


## the parameter "x" is the list that came from "makeCacheMatrix".
## cacheSolve's job is to determine if a cached matrix inverse value
## is still valid, and if so, return it.  Otherwise recompute the inverse
## and order the matrix cache functions to cache this new value.

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
