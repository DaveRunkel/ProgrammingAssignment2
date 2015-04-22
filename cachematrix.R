## Creates matrix inversion cache and matrix inversion function

## creates inverse matrix cache
makeCacheMatrix <- function(x = matrix()) {
  #initializes m as a null matrix
  m <- NULL 
  #sets x to input matrix
  set <- function(y= matrix()){
    x <<- y 
    m <<- NULL     
  }
  ## defines functions in list
  get <- function () x
  setinv <- function (inv) m <<- inv
  getinv <- function () m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## inverts matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Determines if inverse has been calculated and cached already.
  m <- x$getinv()
  if (! is.null(m)){
    message ("Getting Cached Data!")
    return(m)
  }
  # if not cached, calculate inverse
  data <- x$get()
  m <- solve(data)
  #sets inverse
  x$setinv(m)
  m
}
