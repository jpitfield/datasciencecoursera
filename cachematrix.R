###################################################################################
# Author - J Pitfield                                                             #
# Date: 1/10/2015                                                                 #
# Definition: makeCacheMatrix is a function that returns a list of functions      #
# to be able to store a matrix and a cached value of the inverse of the matrix.   #
# Assumptions: The matrix supplied is always invertable                           #
# Functions:                                                                      #
# - setMatrix      set the value of a matrix                                      #
# - getMatrix      get the value of a matrix                                      #
# - cacheInverse   set the cached value of the inverse                            #
# - getInverse     get the cached value of the inverse                            #
# Usage:                                                                          #
# b <- makeCacheMatrix(matrix(c(10,20,23,42), nrow = 2, ncol = 2))                #
################################################################################### 

makeCacheMatrix <- function(x = matrix()) {
  # initially nothing is cached so set it to NULL
  cache <- NULL
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  # returns the stored matrix
  getMatrix <- function() {  
    x
  }
  # cache the inverse 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  # get the cached inverse
  getInverse <- function() {
    cache
  }
}  

###################################################################################
# Author - J Pitfield                                                             #
# Date: 1/10/2015                                                                 #
# Definition: cacheSolve() returns a matrix that is the inverse of x              #
# returns a cached value if exists or generates a new one and caches it           #                                                        #
#                                                                                 #
# Usage:                                                                          #
# cacheSolve(b)                                                                   #
################################################################################### 

cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # check for Is Not Null and return cache if TRUE
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- y$getMatrix()    #Else get the matrix
  inverse <- solve(data)   #caclulate the inverse
  y$cacheInverse(inverse)  #store it in the cache
  
  inverse  # return the inverse
}
