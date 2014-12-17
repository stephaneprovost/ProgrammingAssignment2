## This is an example of how to store a pre-computed result in an object.

## Examples:
## > system.time(matObj<-makeCacheMatrix(diag(2000)*5))
## user  system elapsed 
## 0       0       0 
## > system.time(cacheSolve(matObj))
## user  system elapsed 
## 1.64    0.00    1.64            << slow operation...
## > system.time(cacheSolve(matObj))
## getting cached data
## user  system elapsed 
## 0       0       0               << using cached data!

## This function returns a list containing the following:
## * the set() function which allows us to store the input parameter
##   This is not used here
## * the get() function which retrieves the input parameter
## * the setinverse() which assigns the inverse matrix
## * the getinverse() which returns the inverse matrix
## The function is a really just a place holder of data.
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


## This function will atempt to extract the inverse of the object passed in
## If the inverse matrix has never been calculated, we'll call solve().
## Otherwise, return the cached result from the object.
## Note that x is not a simple matrix, but a matrix returned by the 
## makeCacheMatrix() function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
