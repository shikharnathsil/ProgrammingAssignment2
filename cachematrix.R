## makeCacheMatrix defines functions to cache inverse of
## matrix, so that it can be reused.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y ## store value of matrix in this environment
      inv <<- NULL ## store value of matrix in this environment
    }
    
    get <- function() { x }
    
    setinv <- function(inverse) { inv <<- inverse }
    
    getinv <- function() { inv }
    
    list ( get=get,set=set,
           getinv=getinv, setinv=setinv)
}


## Checks if inverse exist in cache. if Yes, value from
## cache is returned else calculated, stored and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseret <-  x$getinv()
        if(!is.null(inverseret)) {
          message("Getting cached data and NOT recalculating...")
          return(inverseret)
        }
        data <- x$get()
        inverseret <- solve(data)
        x$setinv(inverseret)
        inverseret
}