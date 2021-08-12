## Put comments here that give an overall description of what your
## functions do

## The 'makeCacheMatrix' function creates a matrix object able to save its inverse. 
## The 'cacheSolve' function calcualtes the inverse of the ouput of the 'makeCacheMatrix' if it has not already been calculated.
## If it has, the function retrieves the inverse from chache and returns it.

#### makeCacheMatrix function ####
makeCacheMatrix <- function(x = matrix()) { # Initializes a function 'makeCacheMatrix'
  # that takes as argument a matrix object 'x' 
  s <- NULL                                 # initializes sets the object 's' to NULL
  set <- function(y) {                      # initializes a function 'set' that takes a numeric vector 'y' as argument
    # and in effect "resets" both the 'x' and 's' objects. 
    x <<- y                                 # This function has the same behaviour as the statments above it.
    s <<- NULL
  }
  get <- function() x                       # This "gets' the matrix 'x'
  setsolve <- function(solve) s <<- solve   # This is the setter. It assigns the input to the value that was defined for 's'.
  getsolve <- function() s                  # This function retrieves the values of 's'. 
  list(set = set, get = get,                # This function creates a list of the type 'makeCachaeMatrix'.
       setsolve  = setsolve ,               # It also assigns a name to each element of the list, which is why it can later
       getsolve  = getsolve)                # why it can be accessed by the '$' extractor operator.
}

#### cacheSolve function ####
cacheSolve <- function(x, ...) {            # Initializes the function 'cacheSolve' which takes a matrix as argument, specifically the one created by the above function.
  s <- x$getsolve()                         # Here the function tries to retrieve the inversion of the argument by calling getsolve on the arugment.
  if(!is.null(s)) {                         # Checking that it is TRUE that 's' is not NULL
    message("getting cached data")          # IF this is the case, a message is printed to the console
    return(s)                               # then the value of the inverse stored in 's' is returned
  }
  data <- x$get()                           # IF the above IF loop is close, the function retrieves the input matrix
  s <- solve(data, ...)                     # caluclates the mean of the input matrix
  x$setsolve(s)                             # sets the inverse of the input matrix to 's'
  s                                         # and returns the value of 's' to the parent environment
}