## The following functions will cache the inverse of a matrix 
## instead of computing it repeatedly.

## The first function makeCacheMatrix creates a special "matrix" object  
## that can cache its inverse. It performs the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv_mtx <- NULL
  set <- function(y) {
    x <<- y
    inv_mtx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mtx <<- inverse
  getinverse <- function() inv_mtx
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function cacheSolve calculates the inverse of
## the special matrix created with the makeCacheMatrix function by:
## 1. checking if the inverse has already been calculated
## 2. If yes, gets the cache and skips the computation
## 3. If no, computes the inverse and sets the value of the inverse 
## in the cache via the setInverse function

cacheSolve <- function(x, ...) {
       inv_mtx <- x$getinverse()
  if(!is.null(inv_mtx)) {
    message("getting cached data.")
    return(inv_mtx)
  }
  data <- x$get()
  inv_mtx <- solve(data)
  x$setinverse(inv_mtx)
  inv_mtx
}


## Example
## setting the matrix
x <- rbind(c(1, 3),c(2, 4))

## creating the special matrix using the makeCacheMatrix
m <- makeCacheMatrix(x)

## calculating the inverse since no cache exists
cacheSolve(m)

## getting the cache because the inverse is calculated already
cacheSolve(m)
