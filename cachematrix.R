## This script demonstrates how to cache results in a different environment
## Caching can be usefull for time consuming tasks especially if these tasks are done repeatedly.
## In this case, the example uses a matrix function 

## This function (makeCacheMatrix) creates a list of functions (and returns that list).
## The main reason is to demonstrate lexical scoping.
## The functions created exist when the environment is created

makeCacheMatrix <- function(x = matrix()) {
  ## Return a list of functions focused on applying the solve function to matrices.
  ## this function stores the solution to a different environment (caching)
  ## "The deep assignment arrow, <<-, never creates a variable in the current environment, 
  ##   but instead modifies an existing variable found by walking up the parent environments. "
  ## Quote above extracted from "http://adv-r.had.co.nz/Environments.html"
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  ## return the list of fuctions
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverseMatrix()
  ## checks if the value already had a value (parent environment)
  if(!is.null(m)) {
    message("getting cached data - Inverse Matrix")
    return(m)
  }
  ## if the inverse matrix wasn't cached, get the matrix
  data <- x$get()
  ## calculate the inverted matrix
  m <- solve(data, ...)
  ## set the variable of a parent environment
  x$setInverseMatrix(m)
  ## return the solution (inverted matrix)
  m
}
