## The below code will return the inverse of the matrix

## creates a special "matrix" object that can cache its inverse

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


## computes the inverse of the special "matrix" returned by makeCacheMatrix

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