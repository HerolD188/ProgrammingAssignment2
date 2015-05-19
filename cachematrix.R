## Functions to create a special matrix object that stores a matrix
## and cache's its inverse. 
## Usage
##   z <- makeCacheMatrix(matrix) create a new cached inverse matrix object z
##   cacheSolve(z) calculate and cache the matrix inverse
##
## Usage of cached inverse matrix objects
##   z$get() return the matrix
##   z$set(matrix) replace the matrix value stored by z and set inverse to NULL
##   z$getinv() return the matrix inverse
##   z$setinv(inverse_matrix) cache the inverse matrix

## Create a new matrix object - returns a list of access functions

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(minv1) minv <<- minv1
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Function to calculate and cache the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}
