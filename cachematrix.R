
## The makeCacheMatrix takes a matrix as an input and
## returns a list of functions to set the value of a matrix,
## get the value of that matrix, set the inverse of that matrix, 
## and get the inverse of that matrix.

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


## This function returns the inverse of a matrix either by invoking the solve 
## function or by calling a previously cached result.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
