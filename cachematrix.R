##  a pair of functions that cache the inverse of a matrix

## 'makeCacheMatrix' -This function creates a special "matrix" object that can cache its inverse
## 'makeCacheMatrix' returns a list of functions

# set       set the value of the matrix
# get       get the value of the matrix
# setSolve  set the value of the inverted matrix
# getsolve  get the value of the inverted matrix

# passing x that is accessible to the functions inside makeCacheMatrix i.e.
# 'set','get','setSolve' & 'getSolve'

makeCacheMatrix <- function(x = matrix()) {
    # given below t is the cache and it is set to NULL  
    t <- NULL
    # 'set' function assigns the value 'x1' passed to it to the variable x
    # cache i.e. t is set to NULL
    set <- function(x1) {
        x <<- x1
        t <<- NULL
    }
    # 'get' function returns the variable x
    get <- function() x
    # 'setsolve' function caches the value of the inverted matrix
    setSolve <- function(inverted_matrix) t <<- inverted_matrix
    # 'getsolve' function returns the cached value i.e. t  
    getSolve <- function() t
    # return the list of functions  
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" 
## the matrix was created using 'makeCacheMatrix

cacheSolve <- function(x, ...) {
    # Return a cached value stored in t
    t <- x$getSolve()
    if(!is.null(t)) {
        message("retrieving cached data")
        return(t)
    }
    # if the cache is empty then store the matrix  
    d <- x$get()
    # then get the inverse 
    t <- solve(d, ...)
    # store the inverse in cache
    x$setSolve(t)
    t
}