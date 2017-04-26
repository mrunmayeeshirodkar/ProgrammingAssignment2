## This Function computes inverse of a matrix and stores it in cached data.
## It contains 4 methods 
## set
## get
## setinv
## getinv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function first checks if the inverse of a matrix is been previously 
## computed or not in cached data
## If the function finds inverse of the matrix in cached data then returns
## the previously computed matrix or calculated inverse of matrix and 
## stores in cached data.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
