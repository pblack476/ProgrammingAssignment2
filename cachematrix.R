## makeCacheMatrix() creates an R object that stores a matrix and its inverse. The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve the inverse matrix from the cached value that is stored in the makeCacheMatrix() object's environment.

## makeCacheMatrix() will first clear values for 'm' and define the set() function that sets the matrix x to y on the parent environment. Then it defines the get() function and calls x from the parent environment and defines the setinverse() function which will be the 'setter'and the getinverse() function which will be the 'getter'. It returns a list of those as an input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Takes the list provided by makeCacheMatrix and actually calculates the inverse matrix if it has no cached value 'm'. If it has a cached value 'm' it does not spend resources calculating it and just retrieves it from the makeCacheMatrix()'s environment.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}