## The following functions demonstrate the use of the '<<-' operator
##  The '<<-' operator searches recurively through the environments
##  and tries to assign the value to the the matching variable in the
##  environment that it occours first in the search.
##  However, if the interpreter reaches the global environment before
##  it finds the respective variable it assisgns it to the global
##  environment.

## The function makeCacheMatrix creates a "matrix" object
##  that can cache its inverse.
##  The initialized matrix object is from the type 'list' and  has
##  two attributes:
##    - x, which holds the matrix data and
##    - i, which contains its computed inverse.
##  In addition the object has four 'abilities'/methods:
##    - get: returns the matrix data
##    - set: set the matrix data
##    - setinverse: set the inverse matrix
##    - getinverse: returns the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  i = NULL;
  set <- function(y) {
    x <<- y;
    i <<- NULL;
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse;
  getinverse <- function() i;
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse);
}


## This function returns the inverse matrix.
##  If the matrix already has been computed before,
##  it just returns the received inverse matrix
##  (by calling obj$getinverse()).
##  If the matrix has not been computed before,
##  it first computes the matrix, sets its value
##  to the respective input matrix object (obj$setinverse(inverse_matrix))
##  and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse();
  if(!is.null(i)) {
    message("getting cached data");
    return(i);
  }
  data <- x$get();
  i <- solve(data, ...);
  x$setinverse(i);
  i;
}
