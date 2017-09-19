## makeCacheMatrix creates an object "matrix" which is a list that contains
## functions set, get, setinverse and get invers.
## Once called with an argument of numeric matrix, it creates a list with 
## functions that have this numeric matrix as an argument


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
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
