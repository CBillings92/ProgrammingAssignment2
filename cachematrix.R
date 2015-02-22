## Put comments here that give an overall description of what your
## functions do

## This code creates a function that accepts a matrix as x (or creates an empty matrix)
##should no matrix be supplied by default.This function creates function methods for getting and setting
##function methods that will enable the user to both set and get the inverse of the passed in matrix. The
##function then returns a list of methods so that other functions may call this function and use its methods.
##(what's being used to store the inverted matrix)
##is being set to NULL to declare the variable and set it to NULL as we don't know what the value
##will be just yet. 

makeCacheMatrix <- function(x = matrix()) {
  ##set m to NULL because we don't have a value for the inverted matrix yet and are keeping it vacant. m will represent the inverted matrix. 
  m <- NULL
  ##set method used to set new data (a new matrix) into x instead of having to recall the entire function makeCacheMatrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##a simple get method used to retrieve the value passed into the function and currently stored as variable x
  get <- function() x
  ##a method to set the inverted matrix (of the matrix stored in variable x) and store it in m. the store argument in m is an
  ##argument passed in by calling the cachesolve function.
  setinverse <- function(store) m <<- solve(x)
  ##a simple get method used to return the value of the inverted matrix stored in m
  getinverse <- function() m
  ##build a list of the setter and getter methods. Since this is the last value in the function it is the value that is returned.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function allows for the function (stored with the matrix data) to be passed to it through argument x. 
##this function then checks for any cached data/results within the passed in function. If the function has cached data, the if statement 
##succeeds and returns the value of the cached inverted matrix along with a message to the console. If it doesn't succeed 
##the function calls the passed in function's get method, assigns the results to variable data, then computes the inverse of data
##using the solve function.It then takes these results, and in line 48 calls the passed in function's setinverse method to store
##these results in cache. Finally the function displays the inverted function. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
