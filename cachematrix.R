## Two functions that allow computing the inverse of a matrix
## only once, instead of each time the inverse is needed
##
## This is done by using an additional structure, in form of a list,
## which can store the inverse matrix and return it when needed.
## The inversion routine is then called on the new structure,
## so that it can be checked whether the inverse matrix
## has already been computed.

## Create a matrix that can cache its inverse (if it exists)
## The new object has special properties expressed by functions:
## - set        : sets    the value   of the matrix
## - get        : returns the value   of the matrix
## - setinverse : sets    the inverse of the matrix
## - getinverse : returns the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a matrix (if it exists)
## If the matrix is not invertible, an error is raised

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## No need to re-compute if already done before
    if (!is.null(inv)) {
        message("Getting cache data...")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}
