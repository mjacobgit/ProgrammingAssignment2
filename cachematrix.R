## Put comments here that give an overall description of what your
## functions do
"makeCacheMatrix is used to initialize a matrix and a list of functions that will be leveraged
by cacheSolve function which will be used to calculate the inverse of a matrix"

makeCacheMatrix <- function(x = matrix()) {
  ## Write a short comment describing this function

"The makeCacheMatric function is used to initialize a matrix and also a set of functions 
that will be stored as a list and invoked by the cacheSolve function later."


  i <- NULL
  make <- function(y) { #funtion to initialize the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x   #defining functions to get,setinverse and getinverse of the matrix
  setinverse <- function(h) i <<- h 
  getinverse <- function() i
  list(make = make, get = get,  #making a list of the functions and providing names which 
       setinverse = setinverse, #can be used in function calls.
       getinverse = getinverse)
}


## Write a short comment describing this function

"cacheSolve function is used to solve for the inverse of a matrix initialized by makeCacheMatrix. 
If inverse was previously calculated and cached, the function will retrieve the cached inverse."

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {      #checking if the inverse was previously calculated and cached
    message("getting cached inverse data")
    return(i)
  }
  data <- x$get()              #code to calculate the inverse of the matrix
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
