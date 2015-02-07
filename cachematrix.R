## Put comments here that give an overall description of what your
## functions do

## This function creates an object that can cache its inverse matrix,
## it return a list of functions that are described below

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Clean the cache
        set <- function(y) { # Clean the cache & and assign to x values when
                x <<- y      # you call makeCacheMatrix without arguments
                m <<- NULL
        }
        get <- function() x # Return the value of x
        setinverse <- function(inverse) m <<- inverse # saves into cache
        getinverse <- function() m # retrieve from cache
        
        list(set = set, get = get,    # return a list of the functions set, get
             setinverse = setinverse, # setinverse, getinverse
             getinverse = getinverse)

}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # Retrieve from cache inverse matrix assoc. w/x
        m <- x$getinverse() 
        # Check cache and if it is valid, notify and return it to the user
        if(!is.null(m)) {
                message("getting cached data") 
                return(m)
        }
        # If cache not valid, obtain the matrix
        matrix <- x$get() 
        # Calculate the inverse of the matrix
        m <- solve(matrix, ...)
        # Save the inverse matrix into the cache
        x$setinverse(m)
        # Return the inverse matrix
        m 
}
