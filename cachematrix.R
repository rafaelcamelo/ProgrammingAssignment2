## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions and objects in cache that will be called to solve the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmat <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmat <- function() x 
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m 
        list(setmat = setmat, getmat = getmat,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calls objects and functions to solve the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmat()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## Testing functions

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

cacheSolve(makeCacheMatrix(m1))

