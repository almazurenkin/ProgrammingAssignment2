## Two functions defined below implement mechanism of caching inverse of a matrix.
## We assume in accordance with the task statement that supplied matrix is
## always square invertible matrix, so no validation of input arguments is done.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Note that superassignment (<<-) operator is used in order to pass value 
## to the variable in upper environment.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # initialize inverse setting it to NULL
        
        set <- function (y) { # function set(y) is used to set a new value of original matrix x
                x <<- y # re-defining matrix x by assignin to it matrix y passed as a parameter
                inverse <<- NULL # remember to reset inverse in case it was already set for old value of x
        }
        get <- function () x # function get() returns current value of matrix x
        setinverse <- function (i) inverse <<- i # function setinverse(i) assigns value of matrix i to inverse
        getinverse <- function() inverse # function getinverse() returns value of inverse        

        # now we wrap all four functions into a list that will be returned by function makeCacheMatrix
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse() # fetching inverse of matrix x
        
        if (!is.null(inverse)) { # if inverse is already calculated show message
                message("Using cached data")

        } else { # if inverse isn't cached - calculate, assign to 'inverse' and cache
                inverse <- solve(x$get())
                x$setinverse(inverse)
        }
        
        inverse # returns inverse
}
