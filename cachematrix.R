## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
    }
    get <- function() inv
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set,
         get=get,
         setinv=setinv,
         getinv=getinv
         )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getmean()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setmean(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
