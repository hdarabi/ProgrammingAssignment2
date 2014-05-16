## The first function creates a list. The list contains set and get functions for
## the matrix and set and get functions for inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse for the first time, and return the cached
## version for the sebsequent calls

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get(), ...)
        x$setinv(m)
        m
}
