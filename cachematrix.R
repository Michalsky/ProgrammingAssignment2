## The functions below define a "special matrix" object that provides functionality
## to store a base matrix as well as cache the inverse. Additionally there is a function
## that leverages the cache feature so that if an inverse matrix exists it will not
## recalculate. When setting a new base the inverse is automatically re-initialized
## to NULL so the cached inverse is always tied to the current base matrix.
##
## Assumption 1: Supplied matrices are ALWAYS invertible

## This function creates a special cache "matrix" that can cache its inverse.
## Returns a list or "special matrix" object containing functions to handle
## storing and fetching stored variables with some logic incase the base matrix
## is replaced.

makeCacheMatrix <- function(base = matrix()) {
    # Matrix objectst
    inverse <- NULL
    

    #Set function to set base matrix and re-initialize inverse matrix so the 
    #inverse isn't related to the old base matrix
    set.base.matrix <- function(new_matrix) {
      base <<- new_matrix
      inverse <<- NULL
    }
    #Get function to get base matrix
    get.base.matrix <- function() base
    
    #Cache the inverse of the current base matrix
    set.inverse.matrix <- function(inv) inverse <<- inv
    
    #Get function of the inverse matrix
    get.inverse.matrix <- function() inverse
    
    list(set.base.matrix = set.base.matrix,
         get.base.matrix = get.base.matrix,
         set.inverse.matrix = set.inverse.matrix,
         get.inverse.matrix. = get.inverse.matrix)
}


## This function computes the inverse of the special "matrix". If the inverse 
## has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(cacheMatrix, ...) {
  #Get what is stored as an inverse in the cache matrix
  inverse <- cacheMatrix$get.inverse.matrix()
  
  #Inverse matrix is not cached
  if(!is.null(inverse))
  {
    message("Getting cached inversed matrix")
    return(inverse)
  }
  
  #Cached inverse not found so we will get the current "base" matrix, calculate
  #the inverse, and store it
  base <- cacheMatrix$get.base.matrix()
  inverse <- solve(base, ...)
  cacheMatrix$set.inverse.matrix(inverse)
  
  message("Inverse not cached!! Calculating...")
  inverse
}


