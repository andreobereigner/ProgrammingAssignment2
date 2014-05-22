## Application of R's Scooping Function
#
# The set of functions will calculate an inverse matrix only once and 
# store the inversed matrix in cache for future access in order to avoid
# the repeated (often time consuming) calculation of the same inverse matrix.
#
# Only when the original matrix changes will the functions calculate
# the inversed matrix again.
#
##


## The makeCacheMatrix function stores and returns the original
## and inversed matrix, depending on what method is being called.

makeCacheMatrix <- function(x = matrix()) {
  
  originalMatrix <- x
  inversedMatrix <- NULL
  
  # Define the setter method and store the original matrix
  set <- function(y) {
    originalMatrix <<- y
    inversedMatrix <<- NULL
  }
  
  # Define the getter method and return the original matrix
  get <- function() originalMatrix
  
  # Define the setter method and store the inversed matrix
  setInverseMatrix <- function(inverseMatrix) {
    inversedMatrix <<- inverseMatrix
  }
  
  # Defines the getter method and return the inversed matrix
  getInverseMatrix <- function() inversedMatrix
  
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


## The cacheSolve function checks whether the submitted 
## variable already contains an inversed matrix and returns it if TRUE.
#  When no inversed matrix is available, the method will extract
# the original matrix, create a inverse matrix, store it and return it.

cacheSolve <- function(x, ...) {
  
  # Assume that x already ontains the inversed matrix
  # and extract/store the inversed matrix
  inversedMatrix <- x$getInverseMatrix()
  
  # Check if an inversed matrix was indeed returned
  # (if not the variable will be empty/NULL),
  # and return the inversed matrix
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  
  # If an inversed matrix does not yet exist,
  # extract the original matrix, create inversed matrix
  # and store it. At last, return the inversed matrix.
  data <- x$get()
  inversedMatrix <- solve(data, ...)
  x$setInverseMatrix(inversedMatrix)
  inversedMatrix
  
}
