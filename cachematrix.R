## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.


## The first function "makeCacheMatrix" creates a vector which is also a list containing setter and getter functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) m <<- solveMatrix
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# The following function calculates the inverse of the matrix created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
# value of the inverse in the cache via the setInverse function.

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
