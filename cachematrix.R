## These functions will cache and solve the inverse of a matrix to reduce
## costly computation.

## The function makeCacheMatrix creates a special matrix object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Sets the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Retrieves the matrix
    get <- function() x
    ## Solves for the inverse matrix
    setinverse <- function(solve) m <<- solve
    ## Function retrieves the inverse of the matrix
    getinverse <- function() m
    ## Creates a list of cached elements
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve retrieves the inverse matrix from the cache created
## by the function makeCacheMatrix or computes inverse matrix if it is not 
## cached.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## Checks if the inverse is already cached
    if(!is.null(m)) {
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    ## Calculates the inverse of the matrix
    x$setinverse(m)
    ## Returns the inverse of the matrix
    m
}
