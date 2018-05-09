## The following functions are to create a special matrix, calculate its inverse, and store them in cache, 
## rather than calculating the inverse everytime it is required.  

## makeCacheMatrix() function creates a special "matrix", and has the follwoing functionalities:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
	get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() function is for calculating the inverse of a matrix.
## It first checks if the inverse of a matrix exists, if yes, then it get the value of inverse from cache and 
## returns the inverse.
## If the inverse does not exist, then it finds the inverse using solve() function of R, sets 
## the value of inverse in cache, and aslo returns the inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  x$getinverse()
}

