## R Programming, Week 3
## Programming Assignment 2
## 5.22.14

## Put comments here that give an overall description of what your
## functions do
# These two functions cache the inverse of a metrix.

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse)
            inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse, 
           getinverse = getinverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  
        # If the inverse is already calculated, return it
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
  
        # The inverse is not yet calculated, so we calculate it
        data <- x$get()
        inv <- solve(data, ...)
  
        # Cache the inverse
        x$setinverse(inv)
  
        # Return the inverse
        inv
}
