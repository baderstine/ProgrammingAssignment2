## This is the Coursera RProgramming course assignment #2
# This second programming assignment will require you to write an R function
# that is able to cache potentially time-consuming computations. 
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly (there
# are also alternatives to matrix inversion that we will not discuss here). Your
# assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:
# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 
# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the inverse
# from the cache. 
# Computing the inverse of a square matrix can be done with the solve function
# in R. For example, if X is a square invertible matrix, then solve(X) returns
# its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.

# The first function, makeCacheMatrix creates a special "matrix", which is really a
# list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

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

# The following function calculates the mean of the special "matrix" created
# with the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse from the cache and skips the
# computation. Otherwise, it calculates the inverse of the data and sets the value
# of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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