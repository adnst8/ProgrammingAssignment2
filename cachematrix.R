
## Write a short comment describing this function

## The makeCacheMatrix function (main function) creates a special matrix object that
## caches the input matrix (assumed here to be a square matrix which is invertable) 
## and its inverse.
## The special matrix object is a list containing the following four functions:
## 1. setmatrix - sets the  matrix (by super assigning y to x (x<<-y)) and stores it 
## in the main function (caches it). Using Lexical scoping rules, R first searches for y 
## within the "setmatrix" function but since x was first defined in the main function
## (global environment),it finds it there.
## 2. getmatrix - returns the matrix stored by "setmatrix" in the main function
## 3. setmatrixinv - stores the inverse in a variable "i" in the main function
## 4. getmatrixinv - returns the inverse stored by"setmatrixinv" in the main function

makeCacheMatrix<- function(x=matrix()) {
  i <- NULL           
  setmatrix <- function(y) {
    x <<- y 
    i <<- NULL
  }
  getmatrix <- function() x
  setmatrixinv <- function(inverse) i <<- inverse
  getmatrixinv <- function() i
  list(setmatrix = setmatrix, getmatrix = getmatrix, setmatrixinv = setmatrixinv, getmatrixinv = getmatrixinv)
}

## CacheSolve is a function that calculates the inverse of the special matrix returned by
## makeCacheMatrix function above. It checks to see if the inverse has
## already been calculated. If it has, then it returns the inverse from the cache. 
## However,if the input(matrix) has changed,it calculates the inverse of the matrix,
## caches it, and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  i <- x$getmatrixinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$getmatrix()
  i <- solve(m, ...)
  x$setmatrixinv(i)
  i
}