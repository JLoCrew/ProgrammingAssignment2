## the first function is supposed to create a special "matrix" object that can cache
## its inverse

## the example given was mean, took me a long time to figure it out, but all you
## have to do is to replace mean with the solve function

## solver(A) will give you the inverse of A if A is a square matrix


makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function (y) {
          x <<- y
          inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function () inv
    list(set=set, get=get, 
        setinv = setinv,
        getinv = getinv)

}


## The second part is to cache the inverse matrix instead of computing it every time

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
