
## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = numeric()) {  
  # stores the cached value
  # initialize to NULL
  cache <- NULL
  # create the matrix in the working environment
  set <- function(y) {
    
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    cache <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  # invert the matrix and store in cache
  set_matrix <- function(inverse) cache <<- inverse
  
  # get the inverted matrix from cache
  get_inverse <- function() cache
  
  # return the created functions to the working environment
  list(set = set, get = get,
       set_matrix = set_matrix, get_inverse = get_inverse)
  }


## The following function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) { 
  
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  ## attempt to get the inverse of the matrix stored in cache
  cache <- x$get_inverse()
  
  # if the inverse has already been calculated
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if(!is.null(cache)) {
    # get it from the cache and skips the computation. 
    
    message("getting cached data")
    # display matrix in console
    return(cache)
  }
  # otherwise, calculates the inverse 
  # create matrix since it does not exist
  matrix <- x$get()
  inv <- solve(matrix, ...)
  
  # set inverted matrix in cache
  x$set_matrix(cache)
  
  # display matrix in console
  return(cache)
  
  }





