## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve returns the inverse of the matrix

## function makeCacheMatrix
## creates a special matrix that can cache the input matrix "x" and its inverse matrix "m"
## first, it resets the value of the matrix "x" to the matrix supplied in the main argument
## and resets the inverse matrix "m" to null
## second, it returns the stored matrix
## third, setinverse assigns the value to "m"
## fourth, getinverse returns the value of "m"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## function cacheSolve
## computes the inverse of matrix, if already calculated then it gets inverse from cache and skips calculation
## first, it checks whether cache is full and returns the cached inverse "m"
## if inverse is not cached, then calculates the inverse "m"

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

