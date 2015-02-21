## To cache the inverse of a matrix, implement two functions.
## One (makeCacheMatrix) is the factory (generator) function
## that creates container of a matrix and its inverse.
## The other (cacheSolve) is the function that calculates
## and caches its inverse.
## If cacheSolve is called more than twice for the same matrix,
## cacheSolve returns the cached inverse which is saved in the matrix.

## makeCacheMatrix returns the object
##                 which contains both given matrix and its inverse.
##   NOTE: this object does not caculate the inverse of its given matrix.
## Methods (as list of functions):
##   $set(y) : sets given matrix
##   $get() : returns given matrix
##   $setInverse(inv) : sets the inverse matrix 
##   $getInverse() : returns the inverse matrix if it exists,
##                   returns NULL if it does not exist.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL       # place to cache the inverse of matrix x.
    set <- function(y) {
        x <<- y     # set given matrix as this matrix, x.
        i <<- NULL  # reset the cache, because this matrix has been changed.
    }
    get <- function() x         # returns this matrix.
    setInverse <- function(inverse)
        i <<- inverse           # save the inverse to the cache.
    getInverse <- function() i  # returns the cache
    
    # make the methods as a list and return.
    list(set=set, get=get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'
##            which is made by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()          # check if x cached its inverse.
    if(is.null(i)) {             # if x did not cached its inverse,
        i <- solve(x$get(), ...) # calculate the inverse by solve()
        x$setInverse(i)          # and cache the inverse matrix to x.
    }
    i                            # return the inverse matrix.
}
