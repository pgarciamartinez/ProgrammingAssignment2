## Put comments here that give an overall description of what your functions do
## _______________________________________________________________________________________________

## 'makeCacheMatrix' creates a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## 'cacheSolve' returns the inverse of a matrix

## In order to do this, it can happen that the inverse matrix is cached already or that it does not exist yet
## The latter case means that the 'solve' function needs to be used to calculate the inverse matrix


## Write a short comment describing this function
## _______________________________________________________________________________________________

## As it was shown for the function makeVector in the problem statement, the first function here 
## (makeCacheMatrix) creates a list containing a function to (as mentioned above):

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## For this assignment, the matrix supplied is always invertible.

  inverse_mat <- NULL ## initialises the inverse matrix that will be provided below
  
  set <- function(y) {
      x <<- y ## '<<-' operator assigns value to object in different environment from current one
      inverse <<- NULL ## <<- operator assigns value to object in different environment from current one
  }
  get <- function() x
  
  set_inverse <- function(solve) inverse_mat <<- solve ## 'solve' is the function to generate the inverse in R
  get_inverse <- function() inverse_mat
  
  ## 4 outputs are returned when calling this function
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
## _______________________________________________________________________________________________

## 'cacheSolve' returns the inverse of a matrix
## In order to do this, it can happen that the inverse matrix is cached already or that it does not exist yet
## The latter case means that the 'solve' function needs to be used to calculate the inverse matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inverse_mat <- x$get_inverse()
  
  ## When a matrix is already cached
  if(!is.null(inverse_mat)) {
      message("Getting cached matrix")
    
      return(inverse_mat)
  }
  
  ## When there is no matrix in the cache memory
  data <- x$get()
  
  inverse_mat <- solve(data, ...)
  x$set_inverse(inverse_mat)
  
  ## Returns the inverse matrix
  inverse_mat
}

## Test cases

## Generating simple matrix 2x2 and values from 1 to 4 
test_matrix <- matrix(1:4, nrow = 2, ncol = 2)

## First call to the 'makeCacheMatrix' function created
m <- makeCacheMatrix(test_matrix)

## First call to the 'makeCacheMatrix' function created
cacheSolve(m)

## The resulting matrix should be:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Second call to the 'makeCacheMatrix' function created means there should be a cached version
cacheSolve(m)

## This should show "Getting cached matrix" initially
## And the resulting matrix should be the same as the previous one:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5