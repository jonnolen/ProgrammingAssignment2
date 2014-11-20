## functions supporting caching computationionally expensive operations on matrix objects

## create a special matrix wrapping list.
## list exposes the following functions
##  $get()              returns the last set matrix or the matrix that the list was created with.
##  $set()              replaces matrix, clears any cached values.
##  $setInverse()       sets the cached inverse for the matrix.
##  $getInverse()       retrieves the cached invers for the matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    invisible(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## accepts a cacheMatrix list applies solve() to the matrix contained in the list
## if there is not already a cached result.  otherwise returns cached result
## for solve()
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)){
        message("returning cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
