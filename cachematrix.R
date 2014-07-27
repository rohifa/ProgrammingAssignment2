## This function create a special "cache matrix" object that can cache its inverse.
# This object is a list of functions that
# 1. set the value of the matrix 
# 2. get the matrix
# 3. computes and set the inverse
# 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inverse <<- solve
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
# if the matrix inverse has been already calculated, returns the cached inverse
# otherwise it calculates the inverse, set it in the cache and returns it

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinv()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinv(inverse)
    inverse
}
