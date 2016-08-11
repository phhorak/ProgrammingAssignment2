## a simple way to cache the inverse of a matrix


## makeCacheMatrix initializes a list containing function that sets and gets a matrix and it's inverse 

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

## cacheSolve: input a cached matrix created with makeCacheMatrix and output its inverse.
## if the inverse wasn't calculated before it gets cached.
## if the inverse is already cached, it is just read from cache instead of computed again.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## usage example: 
## x <- makeCacheMatrix(matrix(rnorm(300*300),300,300))
## cacheSolve(x)
## cacheSolve(x)

## first instance of cacheSolve(x) computes the inverse, second instance reads it from cache
