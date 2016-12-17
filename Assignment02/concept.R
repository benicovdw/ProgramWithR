## Create a matrix object that can cache its inverse
## makeCacheMatrix contains 4 functions: set, get, setinverse and getinverse.
## set puts the received matrix into x and NULL into the inverse m
## get returns the matrix stored in x

makeFuncList <- function(x) {
    s <- x
    q <- x^2
    h <- x/2
    i <- 1/x
    list(self = s, quad = q, half = h, inv = i)
}

## cacheSolve CALCULATES the inverse of the matrix
## except if it HAS BEEN done already then it retrieves it from the cache.
## To calculate the inverse 'data' gets the matrix from makeCacheMatrix,
## m calculates the inverse, and x$setinverse(m) stores it back into m
## from makeCacheMatrix.

implement <- function(x, ...) {
    m <- x$getinverse()     ## get the inverse from the cache
    data <- x$get()         ## get the matrix x from the cache
    m <- solve(data, ...)   ## calculate the inverse into m
    x$setinverse(m)         ## store m in the cache
    m                       ## return m
}

## test it
a <- diag(5,7)
b <- makeCacheMatrix(a)
cacheSolve(b)
cacheSolve(b)



