##The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
##1.set the value of the Matrix
##2.get the value of the Matrix
##3.set the value of the Inverse of Matrix
##4.get the value of the Inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
	## m is to store the inverse of matrix and initialized to NULL
        m <- NULL
	## a function to set value of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	## a function "get" to get the value of the Matrix
        get <- function() x
	## a function "setSolve" to set the value of the Inverse of Matrix
        setSolve <- function(Inversed_Matrix) m <<- Inversed_Matrix
	## a function "getSolve" to get the value of the Inverse of Matrix
        getSolve <- function() m
	## list containing all the functions mentioned
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## function calculates the Inverser of the special "Matrix" created with the above function. 
## However, it first checks to see if the Inverse has already been calculated. If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the matrix and sets the value of the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...){
	## retrieve the value of inverse of the matrix to see if it is calculated
	  m <- x$getSolve()
	## check if it is calculated if it is calculated return it from cached data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## if it is not, calculate the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}