## These functions are used to cache the inverse of a matrix. 

## Create a special matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ##begin by setting the inverse to NULL as placeholder
    set <- function(y){
        x <<- y
        i <<- NULL
    } ## define a function to set the matrix, x, to a new matrix, y, and resets the inverse to NULL
    get <- function() x ##returns the matrix, x
    setinverse <- function(solve) i <<- solve ##sets the inverse, i, to solve 
    getinverse <- function() i ##returns the inverse, i 
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## Computes inverse of the matrix returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i 
}
