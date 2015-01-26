## The following function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set the value of the matrix
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        setinverse <- function(matrix) i <<- solve(x)
        ## get the value of the inverse
        getinverse <- function() i
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
