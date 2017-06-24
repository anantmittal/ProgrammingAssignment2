## Put comments here that give an overall description of what your functions do

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Our assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}


#A = matrix(c(1, 2, 3, 4),nrow=2,ncol=2,byrow = TRUE) 
#A
#solve(A)
#makeCacheMatrix(A)
#cacheSolve(makeCacheMatrix(A))
