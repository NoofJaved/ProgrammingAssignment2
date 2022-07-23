## Put comments here that give an overall description of what your
## functions do 
## there are overall two function makeCacheMatrix and cacheSolve
## the makeCacheMatrix consists of set, get,setinv,getinv

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x #  function to get matrix x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv #function to get inverse of matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## used to Cache the data

cacheSolve <- function(x, ...) {
 inv <- x$getinv()
  if(!is.null(inv)) {               #checking whether inverse is NULL
    message("getting cached data")
    return(inv) # return inverse value
  }
  data <- x$get()
  inv<- solve(data, ...) # calculate inverse value
  x$setinv(inv)
  inv
        
}
