## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly. The pair of functions below 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Variable for storing the cached inverse
    inv <- NULL
    ## Four member functions for accessing the matrix and its inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    ## Return a list of the four accessor functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix() above. If the inverse has
## already been calculated (and the matrix has not changed),
## then cacheSolve() retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    ## Check if inverse has already been computed
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Inverse had not yet been computed. Compute inverse
    ## and store it in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
