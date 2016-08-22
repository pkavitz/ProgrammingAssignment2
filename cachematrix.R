##  Assignment submission for Paul Kavitz
##  22-Aug-2016
##
##  Solve inverse of a matrix with a caching mechanism to
##    improve performance for large matrices or for looped
##    iterations using the same inverse matrix.

## The function makeCacheMatrix takes a matrix (x) as a
##    agument and returns an object that includes the matrix
##    and allowable operations upon that data including:
##        x$get() -> returns the matrix
##        x$set() -> instantiates the object with the matrix
##                   and clear the cached inverse (set to NULL)
##        X$setinverse() -> instantiates the inverse of a matrix
##        x$getinverse() -> returns the cached inverse if exists

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve() uses the makeCacheMatrix function
##    to perform the inverse of matrix, but use a cached
##    value of the inverse if already established from a
##    previous calculation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
