## These two functions work together using the solve function to
## cache the inverse of a matrix object created with the first 
## function. The inverse is given as output of the second function, 
## when it is called on the matrix object created by the first function.

## This function creates a special "matrix" object that can cache 
##its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the "matrix" object created 
## by makeCacheMatrix, finding the inverse in the cache if it already
## exists.

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
