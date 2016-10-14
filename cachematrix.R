## These functions cache an matrix and can return the inverse
## of a matrix

## Create a random 4x4 matrix for testing

mat <- matrix(rnorm(16), nrow=4, ncol=4)

## Create a special matrix object that caches it's inverse
## Output will be a list of functions

#Define the function and initialize x
makeCacheMatrix <- function(x = matrix()) {
  # Initialize m in the makeCacheMatrix environment
  m <- NULL  
  # Define the set function
  set <- function(y) {
    #Assign y (input argument) to x in the parent environment
    x <<- y 
    # Assign NULL to m in the parent environment
    m <<- NULL 
  }
  # Define the getter function for x which is a matrix
  get <- function() x 
  # Assign the function input to m in the parent environment
  setmatrix <- function(inverse) m <<- inverse 
  # Define the getter function for m
  getmatrix <- function() m 
  # Create list with all the functions
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) 
}

## Return the cached version of matrix if it exists; otherwise calculate it

#Define the cacheSolve function
cacheSolve <- function(x, ...) {
    # Call the getmatrix function from makeCacheMatrix
    m <- x$getmatrix()
    # Test if an existing matrix inverse has already been calculcated
    if(!is.null(m)) {
      message("getting cached data")
      # Return the value of the cached matrix if it exists
      return(m)
    }
    # If a cached matrix does not exists, calculate the matrix inverse
    matrix <- x$get()
    # Invert the matrix and assign to m
    m <- solve(matrix, ...)
    #Call the setmatrix function from makeCacheMatrix
    x$setmatrix(m)
    # Return the value of the newly calculate inverse matrix
    return(m)
  }