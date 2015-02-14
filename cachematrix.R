## The purpose of this project is to write a pair of functions to reduce the 
## amount of computations required to compute the inverse of a matrix. We will
## save a computed inverse matrix so that, if one wants to access it more than
## once, there is no need to compute it every time. If the original matrix is
## changed, the inverse matrix will turn to NULL in order to avoid mistakes.

## This function creates a special matrix, which is actually a list containing
## four functions:
## set(): To set the value of the matrix.
## get(): To get the value of the matrix.
## setInverse(): To set the value of the inverse matrix.
## getInverse(): To get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse matrix of a special matrix like the one
## above. If the original matrix is not changed, it will return the old value
## and avoid superfluous computations. Otherwise, it will compute it, save it
## for later uses, and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
