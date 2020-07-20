##makeCacheMatrix: creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   ##function declaration
  i <- NULL                                   ##null object declaration
  set <- function(y) {                        ##function to set matrix value
    x <<- y
    i <<- NULL
  }
  get <- function() x                         ##get matrix value
  setInv <- function(inverse) i <<- inverse   ##set inverse matrix
  getInv <- function() i                      ##get inverse value
  list(set = set, get = get,                  ##store original or inverse values
       setInv = setInv,
       getInv = getInv)
}


##cacheSolve: computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated & the matrix 
##has not changed then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {             ##function declaration
  i <- x$getInv()                            ##assigns inverse matrix
  if(!is.null(i)) {                          ##returns cached data with a 
    message("getting cached data")           ##message if i is not null
    return(i)
  }
  data <- x$get()                            ##get the original matrix value 
  i <- solve(data, ...)                      ##inverse matrix
  x$setInv(i)                                ##set inverse matrix in cache
  i
}
