## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmetric <- function(solve) m <<- solve
  getmetric <- function() m
  list(set = set, get = get,
       setmetric = setmetric,
       getmetric = getmetric)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmetric()
  if(!is.null(m)) {
    message("getting cached metric")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmetric(m)
  m
}