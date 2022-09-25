## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix contains set, get, setinv and getinv
##library MASS is used to calculate squared and non squared matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL    #inverse as null
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x   #this function gets matrix x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() {
                          inver <-ginv(x)
                          inver %*% x          #this function helps obtain the inverse of the matrix 
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## Write a short comment describing this function
## used to describe the cache data
cacheSolve <- function(x, ...) #this helps get cache data
  {
  inv <- x$getinv()
  if(!is.null(inv)) {             #checks whether the inverse is null
                     message("getting cached data!")
                      return(inv)   #returns inverse value
                     }
  data <- x$get()
  inv <- solve(data, ...)                 #calculates inverse values
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
