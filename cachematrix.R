## This function generates a matrix of random numbers. However, 
## it will check if the reverse is received first.


makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function returns the inverse of matrix giving by
## function makeCacheMatrix. 

cacheSolve <- function(x, ...) {i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        }
