## Input is a square matrix. The goal is to find its inverse.
## If matrix has not changed, retrieve cached inverse.

## makeCacheMatrix creates a list, containing functions to
## reset variables ("set"),
## retrieve the input matrix ("get"),
## find the matrix inverse ("setInverse"), and
## retrieve an existing matrix inverse ("getInverse").

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve checks if the matrix inverse is empty,
## if not, retrieves the existing matrix inverse and returns it,
## if yes, calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
