## These functions creates a special matrix object
## that can cache its inverse.

## This functions creates a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function checks if cache contains the solved matrix
## if not it solves the matrix and puts it in the cache (m)

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

##to test:
m <- makeCacheMatrix(matrix(1:4, nrow=2))
cacheSolve(m)
cacheSolve(m)

