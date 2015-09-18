## Put comments here that give an overall description of what your
## functions do

## This function creates a list of 4 objects which contains the original matrix and 
## a placeholder for the inverse. This needs to be used in conjunction with 
## cacheSolve function
## Input has to be a square matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { 
        ##By using <<- operator, we can pass value from the calling function
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
## For computing the inverse, the matrix has to be a square matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) { 
        #Tests if the inverse is already computed, if so use cached data
        message("getting cached data")
        return(m)
    }
    ## Otherwise, get the original matrix and compute the inverse. Store it in $setinv
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
