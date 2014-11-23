## Matrix inversion is usually a costly computation and their may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. The following pair of functions
## compute and cache the inverse of a matrix:
##   1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##   2. cacheSolve: This function computes the inverse of the special "matrix" returned by the
##      makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not
##      changed), then the cacheSolve function will retrieve the inverse from the cache.



## The makeCacheMatrix function creates a special "matrix", which is really a list
## containing the following functions:
##   1. set the value of the matrix (set)
##   2. get the value of the matrix (get)
##   3. set the value of the inverse (setinverse)
##   4. get the value of the inverse (getinverse)
## This function takes advantage of the scoping rules of the R language and how they can be
## manipulated to preserve state inside of an R object using the <<- assignment operator.

makeCacheMatrix <- function(x = matrix()) {
    ## clear cached value of inverse
    inv <- NULL
    
    ## the following function sets the value of the matrix and clears the cached value of the
    ## inverse using the <<- assignment operator to set the variables in the parent environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## the following function gets the value of the matrix (from the parent environment)
    get <- function() x
    
    ## the following function sets the cached value of the inverse using the <<- assignment operator
    ## to set the vairable in the parent environment
    setinverse <- function(z) inv <<- z
    
    ## the following function gets the value of the inverse (from the parent environment)
    getinverse <- function() inv
    
    ## the following creates the list of functions available in makeCacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrx" created with the
## makeCacheMatrix function (above). However, it first checks to see if the inverse has
## already been calculated and saved in the cache. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data matrix and
## sets the value of the inverse in the cache via the setinverse function.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## first check to see if inverse is already in cache...
    ## get current value from inverse cache; if not null, inverse has already
    ## been calculated and stored in cache
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## inverse is not already cached - need to compute inverse...
    ## get matrix data via get function and compute inverse; computing the inverse of a
    ## square matrix can be done with the solve function. For example, if X is a square
    ## invertible matrix, then solve(X) returns its inverse.
    data <- x$get()
    message("calculating inverse of matrix")    
    inv <- solve(data, ...)
    
    ## save calculated inverse in cache via setinverse function before returning inverse
    x$setinverse(inv)
    inv
}
