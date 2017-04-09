## Caching the Inverse of a Matrix
## Some calculations can take significant time to run. 
## In that case it may sense to calc once and store the results
## to be used in the future.
## The 2 functions below store a matrix and cache its inverse.

## This function caches the inverse into a matrix object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}          


## Here is where we calc the inverse of a matrix if necessary
## or we use the cache data if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
              return(inv)  
        }  
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
