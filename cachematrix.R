# Below are two functions that are used to create a special object 
# that stores a numeric vector and cache's its inverse.
## functions do

#makeCacheMatrix creates a special "matriz", w
#hich is really a list containing a function to

#set the value of the matriz
#get the value of the matriz
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculates the inverse of the special "matriz"
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

x <- c(3,1,6,13,5,4,3,5,7,7,9,4,2,2,17,16)
dim(x) <- c(4,4)
cache <- makeCacheMatrix(x)
cacheSolve(cache)

x <- c(4,2,7,6)
dim(x) <- c(2,2)
cache <- makeCacheMatrix(x)
cacheSolve(cache)
