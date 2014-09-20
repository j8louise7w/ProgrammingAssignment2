## Function to cache the inverse of a matrix

## Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
#create empty matrix to which inverse will be assigned
     i <- NULL
#create function that will serve as value of matrix to be inverted
     set  <- function (y) {
          x <<- y
          i <<- NULL
     }
#create function that will get the value of the matrix
     get <- function() x
#create function that will set the inverse of the matrix
     setinverse <- function(solve) i <<- solve
#create function that will get the inverse of the matrix 
     getinverse <- function() i
     list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Function that calculates the inverse of makeCacheMatrix
##If it already exists, will return cached value, otherwise will calculate and set value

cacheSolve <- function(x, ...) {
#pull in getinverse value from makeCacheMatrix    
     i <- x$getinverse()
#if the value is not null, creturn the i     
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
#if the value is null, solve for the inverse and return the value    
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}