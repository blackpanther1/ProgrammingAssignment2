## makeCacheMatrix function creates a matrix which can cache itself and aslo its inverse
## cacheSolve function calculates the inverse of the matrix returned by makeCacheMatrix and returns that inverse. In case invers has already been calculated and cached it retrieves the cached value and returns it.
if("matlib" %in% installed.packages()){
  require("matlib")
}else{
  install.packages("matlib")
  require("matlib")
}
##  This function creates a special "matrix" object that can cache its inverse
## input of this function is a  square matrix e.g. A <- matrix(c(2, 1, -1,-3, -1, 3,-2,  1, 2), 3, 3)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse<- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- Inverse(data, ...)
  x$setinverse(inv)
  inv
}
