## These functions create a special matrix object which cashes the inverse 
##in creating makeCacheMatrix we need to define, get and set; where we need to get the inverse the function getinverse is applied; whichever is needed, ultimately the inverse matrix is listed

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL ## m clears the cache 
        set <- function(y) {
                x <<- y ## sets the value 
                m <<- NULL
        }
        get <- function() x ## to get the value of the matrix 
        setinverse <- function(inverse) m <<- inverse ## sets the inverse 
        getinverse<- function() m ## to get the inverse of the matrix if it not inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## returns the inverse of matrix x; if the inverse is already done, then cacheSolve just retrieves the information from the cache 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { ## if the cache is not empty, it just returns the matrix in the cache 
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## if it empty or NULL, then the inverse is solved for the value of x 
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
