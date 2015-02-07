## This function creates an object that can cache its inverse matrix,
## it return a list of functions that are described below

makeCacheMatrix <- function(x = matrix()) {
        # Clean m that act as cache
        m <- NULL 
        # Clean the cache and assign to x the value of y 
        # to pass values when makeCacheMatrix is called without arguments
        set <- function(y) { 
                x <<- y      
                m <<- NULL
        }
        # Return the value of x
        get <- function() x 
        # Saves the inverse matrix into cache
        setinverse <- function(inverse) m <<- inverse
        # Retrieve the inverse matrix from cache
        getinverse <- function() m 
        # Return a list of the functions x$set(), x$get(),  
        # x$setinverse() and x$getinverse
        list(set = set, get = get,    
             setinverse = setinverse, 
             getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x' using the functions provided
## by makeCacheMatrix or calculating the inverse matrix if it not in the cache
## and saving it in the cache.
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
