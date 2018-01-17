# See README.md for instructions on running the code and output from it
# The assignment states that running the code is not part of the grading 
# but I have the instructions anyway.

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
#
# Notes:
# not sure how the "x = numeric()" part works in the argument list of the 
# function, but it seems to be creating a variable "x" that is not reachable 
# from the global environment, but is available in the environment of the 
# makeCacheMatrix function
make_cache_matrix <- function(x = numeric()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  set_matrix <- function(new_value) {
    x <<- new_value
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  get_matrix <- function() {
    x
  }
  
  # cache the given argument 
  cache_inverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  get_inverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(set_matrix = set_matrix, get_matrix = get_matrix, cache_inverse = cache_inverse, get_inverse = get_inverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cache_solve <- function(y, ...) {
  # get the cached value
  inverse <- y$get_inverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$get_matrix()
  inverse <- solve(data)
  y$cache_inverse(inverse)
  
  # return the inverse
  inverse
}