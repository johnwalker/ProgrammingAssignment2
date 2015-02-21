## Returns a matrix that contains a cache for its own inverse.
makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL

    ## Sets the matrix and invalidates the current inverse 
    set <- function(y) {
        x <<- y
        cachedinverse <<- NULL
    }
    ## Returns the matrix
    get <- function() x
    
    ## Returns the cached inverse. It will be NULL if
    ## cacheSolve hasn't been called after the matrix was
    ## last set.
    getinverse <- function() cachedinverse
    
    ## Sets the inverse
    setinverse <- function(inverse) cachedinverse <<- inverse
    list(set = set,
         get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}


## Returns the inverse of the matrix. Computes the inverse
## and sets it if it hasn't been solved. Otherwise retrieves
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtx <- x$get()
    inv <- x$getinverse()
    if (is.null(inv)) {
        inv <- solve(mtx, ...)
        x$setinverse(inv)
    }
    inv
}
