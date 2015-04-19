## Cached Inverse matrix functions

## create a cached matrix with get/set functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## calculate the inverse of a matrix or return a cached value if available
## used in conjunction with makeCacheMatrix.
## e.g. 
## > m1 <- matrix(c(0, 10, 20, 0), nrow = 2, ncol = 2)
## > m <- makeCacheMatrix(m1)
## > cacheSolve(m)
## [,1] [,2]
## [1,] 0.00  0.1
## [2,] 0.05  0.0

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
