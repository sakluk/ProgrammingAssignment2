## cachematrix.R - A collection of functions for manipulating cached matrices.


## makeCacheMatrix
##
## Creates a special matrix object that can cache its inverse.
##
## Usage
## 
## makeCacheMatrix(x)
##
## Arguments
##
## x is a matrix() object.
##
## Value
##
## Returns a custom matrix, which is a list containing 4 functions:
## - set = sets the value of the matrix
## - get = gets the value of the matrix
## - setinv = sets the inverse value of the matrix
## - getinv = gets the inverse value of the matrix
##
## Examples
##
## # Create a 2x2 matrix having random numbers
## x <- matrix(rnorm(4), 2, 2)
## A <- makeCacheMatrix(x)
## A$get()
## A$getinv() # Return the NULL value, as the inverse is not set
## B <- solve(A$get())
## A$setinv(B)
## A$getinv() # Now returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(y) xinv <<- y
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve
##
## Calculates the inverse of the custom matrix and stores the value in the custom matrix.
##
## Usage
##
## cacheSolve(A)
##
## Arguments
##
## A is a special matrix created with makeCacheMatrix function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
