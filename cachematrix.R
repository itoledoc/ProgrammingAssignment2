## This file contains two objects that can help to save computing time by
## using a cache. The main idea, in this case, is that the object created with
## makeCacheMatrix will not only store a matrix, but also will store the inverse
## of this matrix once is calculated.


## makeCacheMatrix is an object that can store a matrix and its inverse
## once this one is calculated with the special function cacheSolve.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## makeCacheMatrix$set : store a matrix and reset the inverse value to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## makeCacheMatrix$get : returns the value of the matrix
    get <- function() x
    ## makeCacheMatrix$setinverse : store the inverse of the matrix
    setinverse <- function(solve) m <<-solve
    ## makeCacheMatrix$getinverse : returns the inverse of the matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix stored in makeCacheMatrix,
## either by calculating it, if the inverse has not been calculated yet, or 
## by reading the stored result from a previous calculation.
## If the inverse is calculated for the first time, it will also store the 
## result in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## if the inverse has been already stored in makeCacheMatrix, return the 
    ## stored value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## else, calculate the inverse, store it in the cache and return the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
