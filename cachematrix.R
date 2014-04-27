## Give interface to work with big square matrixes
## to prevent calculation its inverse every time
## when it is required by business logic

## Creates a matrix with API methods get, set, setinverse and getinverse
## from common R matrix, initiates cache storage variable for the inverse
## and gives methods to get and set values to cache storage

makeCacheMatrix <- function(x = matrix()) {
  #initiate cache storage in m variable with NULL (empty)
  m <- NULL
  
  # API method set
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # API method get
  get <- function() x
  
  # API method setinverse fills the cache storage variable
  setinverse <- function(inverse) m <<- inverse
  
  # API method getinverse retrieves the cached inversed matrix
  getinverse <- function() m
  
  # list of available API methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Replace the solve function to optimize performance when we need
## huge square matrix inverse several times in our code, preventing its calculation
## several times.
## x argument should be an interfaced matrix processed by makeCacheMatrix function 
## defined above

cacheSolve <- function(x, ...) {
  # check if we have already cached inverse for the matrix
  m <- x$getinverse()
  
  # if cache is not empty - then pick it from cache and return
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  
  ## cache is empty - then solve the matrix
  mtx <- x$get()
  m <- solve(mtx)
  ## and cache it for further usage
  x$setinverse(m)
  
  ## debug message for tests
  message("getting non-cached matrix inverse")
  
  ## print the inversed matrix
  m
}

## Uncomment lines with R-code to make a test. 
## Please, keep in mind, that the cached value will be retrieved after the second call 
## of cacheSolve method

## mymtx<-makeCacheMatrix(matrix(1:4, 2,2))
## mymtx$get()

## will retrieve non-cached inverse
## cacheSolve(mymtx) 

## will retrieve cached inverse
## cacheSolve(mymtx) 