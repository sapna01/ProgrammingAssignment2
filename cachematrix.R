## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## Initializing the inverse property.
  i <- NULL
  
  ## Method to set the matrix.
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix.
  get <- function() {
    ## Returns the matrix
    m
  }
  
  ## Method to set inverse of the matrix.
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix.
  getInverse <- function() {
    ## Returns the inverse property.
    i
  }
  
  ## Returns a list of the methods.
  list(set = set, get = get,
                       setInverse = setInverse,
                       getInverse = getInverse)
  
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  ## Returns the inverse if already set.
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  
  ## Gets the matrix from object.
  data <- x$get()
  
  ## Calculates inverse using matrix multiplication.
  m <- solve(data) %*% data
  
  ## Sets the inverse to the object.
  x$setInverse(m)
  
  ## Returns the matrix.
  m
  
}
