# Programming-Assignment-2-Lexical-Scoping-6789
This second programming assignment will require you to write an R function that is able to makeCacheMatrix or cache potentially time-consuming computations.
# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
## makeCacheMatrix function
makeCacheMatrix <- function(x = numeric()) {
   
   # holds the cached value or NULL if nothing is cached
   # initially nothing is cached so set it to NULL
   cache <- NULL
   
   # store a matrix
   setMatrix <- function(newValue) {
     x <<- newValue
     # since the matrix is assigned a new value, flush the cache
     cache <<- NULL
   }
   
   # returns the stored matrix
   getMatrix <- function() {
     x
   }
   
   # cache the given argument 
   cacheInverse <- function(solve) {
     cache <<- solve
   }
   
   # get the cached value
   getInverse <- function() {
     cache
   }
   
   # return a list. Each named element of the list is a function
   list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
 }
> 
### The following function calculates the inverse of a "special" matrix created with 
### cacheSolve
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
   
  # return the inverse
  inverse
}
### Result of makeCacheMatrix 
niraj <- makeCacheMatrix(matrix(c(54, 64, 74, 84), nrow = 2, ncol = 2))
summary(niraj)
 ##              Length Class  Mode    
>## setMatrix    1      -none- function
>## getMatrix    1      -none- function
>## cacheInverse 1      -none- function
>## getInverse   1      -none- function
> 
> 
niraj$getMatrix()
##       [,1] [,2]
>##[1,]   54   74
>##[2,]   64   84
> 
cacheSolve(niraj)
##        [,1]  [,2]
>## [1,] -0.42  0.37
>## [2,]  0.32 -0.27
> 
