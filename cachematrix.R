## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.
# The objective is to create a list of functions, containing:
# set -> sets x as a new matrix
# get -> get the original matrix
# set_inv -> sets the inverse matrix
# get_inv -> get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(f){
                x <<- f
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(y) inv <<- y
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
        p <- x$get_inv()
        if(!is.null(p)){
                message("getting cached data")
                return(p)
        }
        data_1 <- x$get()
        data_1_inv <- solve(data_1)
        x$set_inv(data_1_inv)
        data_1_inv
}
