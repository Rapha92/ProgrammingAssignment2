## If applied together, the two functions below return the inverse
## of the matrix that the first function takes as an argument (
## assumed that the matrix is invertible).

## The first function calculates the inverse of the martix that it
## takes as an argument and stores it in the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## The second function retrieves the inverse of the matrix stored
## in the cache (if there is one) and prints it. 
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$
    getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
