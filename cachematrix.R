## These functions will cache and solve the inverse of a matrix to reduce
## costly computation.

## The function makeCacheMatrix creates a special matrix object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve retrieves the inverse matrix from the cache created
## by the function makeCacheMatrix or computes inverse matrix if it is not 
## cached.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
