## makeCacheMatrix and cacheSolve functions calculate the inverse of a matrix and cache it 
## so that it doesn't have to be recomputed everytime
 
## The below function makeCacheMatrix takes a matrix as input and creates a special cacheable matrix 
## and returns a list of functions to
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,       
       getinverse = getinverse)
}


## The below function cacheSolve takes the list returned by the makeCacheMatrix function as input and returns 
## the inverse of the matrix. It first checks if the inverse has already been calculated. If it has been 
## calculated it will return the cached value and skips the computation. If not, 
## it will compute the inverse of the matris and then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
