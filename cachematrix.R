## The following packages contains two functions.
## The first function creates an inverse matrix of a matrix of the users' input.
## The second function calculates an inverse matrix of the users' input, only if
## The matrix has not been inputed before. If it has, then the function
## simply takes the inverse of the said matrix from a cache. 

## To use the following program, you will need to install the matrixcalc package.
## install.packages("matrixcalc")
## library("matrixcalc")

## makeCacheMatrix is a function that creates a special "matrix".
## the function is doing four things:
## 1) setting the value of the matrix
## 2) getting the value of the matrix
## 3) setting the value of the inverse matrix, and
## 4) getting the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(matrix.inverse) m <<- matrixinverse
  getmatrixinverse <- function() m
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}

## cacheSolve is a function thate computes the inverse of the special "matrix"
## if the inverse has not been calculated already.
## If the inverse matrix has been calculated already, then
## cacheSolve does not comput anything
## rather, it takes the inverse matrix from the cache to save computing time.

cacheSolve <- function(x, ...) {
  m <- x$getmatrixinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- matrix.inverse(data, ...)
  x$setmatrixinverse(m)
  m
}