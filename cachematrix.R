## These 2 functions are as per the need of Programming assignment 2 under 
## R programming Course offerred by Coursera.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function checks if there is any value for matrix inverse in cache
## If yes it returns this cached value, 
## otherwise it computes the inverse stores it in cashe and returns this computed value

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}