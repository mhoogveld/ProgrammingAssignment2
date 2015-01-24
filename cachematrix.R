## These functions help in creating a special matrix which can cache it's inverse
## so it does not need to be recalculated when needed the next time

## Create a special matric which can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Retrieve the cached inverse of the matrix or, if not available in cache, calculate
## it and store it in cache
cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached inverse matrix")
        return(s)
    }
    matrix <- x$get()
    s <- solve(matrix, ...)
    x$setSolve(s)
    s
}
