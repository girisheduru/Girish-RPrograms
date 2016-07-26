## To calculate the inverse of the matrix and also allow cache process through the enviroment 


## This function takes the matrix value and and has getters and setter for for the matrix value
#and its inverse value as a result
# This basically has a list of functions(setters and getters) to the matrix values

# The actual inverse function logic is built in the next function 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is the one that gets called intially
# It first checks if the variable evalauted for the inverse is available through getInverse
# in the cache/environment. if yes it would retrieve/get returned from the cache. 
# if its not available then it gets the value of the matix and passes the same t solve 
# function to find the inverse of it and assign it to the result that gets returned as m 
# x is the reference to function makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
