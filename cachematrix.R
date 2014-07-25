## This file contains two functions. The first function creates a matrix and calculates its inverse
## The second function returns the inverse of a matrix and calculates it if the inverse is not yet cached

## makeCacheMatrix takes a matrix (x) as its input and creates a special matrix that does 4 things
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the inverse (inv) of the matrix using solve
## 4. gets the inverse of the matrix (inv)
## the output of makeCacheMatrix should be saved as a variabel (class of value)

makeCacheMatrix <- function(x = matrix()) {
  
  # initializes inv to null
  inv <- NULL
  
  # function that sets the value of the matrix (x) and intializes inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function that gets the value of the matrix (x)
  get <- function() x
  
  # function that solves for the inverse the matrix and saves the result as inv
  setinverse <- function(solve) inv <<- solve
  
  # function that retrieves the inverse of the matrix x (inv)
  getinverse <- function() inv
  
  # saves function in a list so that they can be later called using "$"
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve the output of makeCacheMatrix and returns the inverse
## first it checks to see if the inverse has already been calculated (inv)
## if so, it simply returns the value from get
## otherwise, it calculates the inverse using setinverse and returns the result (inv)

cacheSolve <- function(x, ...) {
  
  # returns whatever value is stored as inverse
  inv <- x$getinverse()
  
  # checks to see if inv is null (not calculated), if not, it returns the stored value of inv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise the function gets the matrix (x), calculates its inverse (inv), and returns it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
