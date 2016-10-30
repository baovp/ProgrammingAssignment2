## Put comments here that give an overall description of what your
## functions do
## Pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
## ---------------Verify the result------------------------
## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)

##            [,1]       [,2]       [,3]       [,4]
## [1,]  0.08435161 -1.9524200  0.4140862 -1.1589769
## [2,] -0.47233389  0.5765368 -0.9438463  0.2020037
## [3,] -0.70373111 -5.8191351  3.6154145 -2.3637234
## [4,]  0.73436478  4.3533774 -2.0467863  1.3741943
