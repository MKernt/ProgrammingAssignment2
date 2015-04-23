###########~R Programmin Assignment 2~####################
## The first part of thefunction, makeCacheMatrix creates a special 
## "matrix", which is a list containing a function to
## 0. initialize the inverse matrix value
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## 5. return a list of all the four functions
makeCacheMatrix <- function(x = matrix()) {
  # 0. initialize the inverse matrix value
  inv <- NULL
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 2. get the value of the matrix
  get <- function() x
  # 3. set the value of the inverse
  set_inverse <- function(inv_input) inv <<- inv_input
  # 4. get the value of the inverse
  get_inverse <- function() inv
  # 5. return a list of all the four functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)  
}

## Following, the function underneath calculates the inverse of the 
## special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.
cacheSolve <- function(x, ...) {
  # check if the inverse is already cached,
  # if so, we get the inverse from the cache directly
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("cache inversed")
    return(inv)
  }
  # else, here we get the matrix
  data <- x$get()
  # calculate the inverse
  inv <- solve(data, ...)
  # cache the inverse of the matrix
  x$set_inverse(inv)
  # return the result
  inv
}