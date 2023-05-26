## return object with 'set', 'get', 'setinverse', and 'getinverse' functions for
## matrices to be cached along with their inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mat_inverse <- NULL
  
  # create 'set' function to cache matrix
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  
  # create 'get' function to get cached matrix
  get <- function() x
  
  # create 'setinverse' function to cache inverse
  setinverse <- function(inverse) mat_inverse <<- inverse
  
  # create 'getinverse' function that gets the cached inverse
  getinverse <- function() mat_inverse
  
  # return object with 'set', 'get', 'setinverse', and 'getinverse'
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  mat_inverse <- x$getinverse()
  
  # if matrix inverse exists, return the cached data
  if(!is.null(mat_inverse)){
    message("getting cached data")
    return(mat_inverse)
  }
  
  # otherwise, get the matrix and solve
  data <- x$get()
  mat_inverse <- solve(data, ...)
  x$setinverse(mat_inverse)
  mat_inverse
  
}
