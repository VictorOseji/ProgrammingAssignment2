## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse as NULL
  inv <- NULL
  
  # Setter function to set the matrix and reset inverse cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Getter function to get the matrix
  get <- function() x
  
  # Setter function for the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function for the inverse
  getInverse <- function() inv
  
  # Return a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Retrieve the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
