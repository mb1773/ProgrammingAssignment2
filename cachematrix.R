## Cachematrix contains a pair of functions.  The first creates a matrix that can 
## cache its inverse.  The second retrieves the inverse of the matrix from the
## cache or computes the inverse if it cannot retrieve the inverse from cache  

## MakeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve computes the inverse of the special matrix that is returned 
## by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
