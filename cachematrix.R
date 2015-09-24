## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then cacheSolve should retrieve the inverse from the cache.

## Input: a general matrix object. It is assumed as an invertible matrix.
## Output: a special matrix object, that can cache its inverse
## Description: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Input: the special matrix object, created with makeCacheMatrix
## Output: the inverse of the input matrix
## Description: computes and caches the inverse of the input matrix. If the inverse has already
##              been calculated and the input matrix has not changed, retrieves its inverse
##              from cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
