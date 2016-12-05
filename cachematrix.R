## Functions that calculate (if not calculated before) and cache the inverse of 
## a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It is a list of functions to set/get the matrix and set/get the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Set the matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    # Get the matrix
    get <- function() x
    # Set the inverse
    setinverse <- function(inverse) inv <<- inverse
    # Get the inverse
    getinverse <- function() inv
    # Return list
    list(set = set,get=get,setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated , then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Check if inverse cached
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # If not cached then calculate and cache inverse
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

