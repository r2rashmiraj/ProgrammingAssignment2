## The objective of this assignmnet is to calculate the inverse of the matrix.
## Since matrix inversion is usually a costly computation so we are taking the advantage of caching the inverse of a matrix
## rather than computing it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. It contains a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

##Here we use the <<- operator to assign the value to an object in an environment that is different from the current environment

## We cache the inverse of variable matrix x in variable inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated i.e. inv is not null, 
## then the cachesolve retrieve the inverse from the cache. 
## If the inverse is not calculated, it uses solve() to calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
