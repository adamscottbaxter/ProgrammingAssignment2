## Calculating the inversion of a matrix can require a large amount of
## processing resources. If the result of a matrix inversion is needed
## multiple times (used in a loop) it would be more economical to cache
## the result in a higher level environment and simply retrieve it when needed.

## makeCacheMatrix creates a function which begins as a null matrix arguement,  
## sets the value of the matrix inversion to NULL, and delcares another 
## function where the value will be cached.

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

## cacheSolve retrieves the previously cached matrix solution and notifies
## the user; otherwise it calculates and returns the solution.

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