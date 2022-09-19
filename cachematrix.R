## The makeCacheMatrix function enables the creation of a matrix.
## An example 2x2 matrix might be: matrix(c(1,2,3,4),2,2).
## The cacheSolve function creates an inverse of the matrix and 
## returns the inverse. 

## The makeCacheMatrix function enables the creation of a matrix.

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


## The cacheSolve function creates an inverse of the matrix and 
## returns the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv  ## Return a matrix that is the inverse of 'x'
}
