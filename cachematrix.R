## Put comments here that give an overall description of what your
## functions do

## This function creates a list of 4 objects which contains the original matrix and 
## a placeholder for he inverse. This needs to be used in conjunction with 
## cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(myinv) m <<- myinv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## Computes the inverse of a matrix. If the inverse is already present, it will 
## use that. Otherwise, it will create a new one. For large matrices this saves
## significant amount of time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
