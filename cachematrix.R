## makeCacheMatrix creates an object "matrix" which is a list that contains
## functions set, get, setinverse and get invers.
## Once called with an argument of numeric matrix, it creates a list with 
## functions that have this numeric matrix as an argument


makeCacheMatrix <- function(x = matrix()) {
## creates empty object m
  m <- NULL
## defines function set that creates local object x that stores argument y
## also creates local definition for object m as empty
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## defines function get
  get <- function() x
## defines function setinverse that takes function solve and fill object with solve
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}





## cacheSolve takes the "matrix" object created by the function above
## and checks whether this object was used to calculate inverse. If it did, than
## this function just takes outputs the cached value. If not - it calculates the inverse
## the matrix again.

cacheSolve <- function(x, ...)  {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
