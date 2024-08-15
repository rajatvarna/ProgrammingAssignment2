## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse when matrix is changed
    }
    
    # Function to get the matrix
    get <- function() {
        x
    }
    
    # Function to set the inverse
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    # Function to get the inverse
    getInverse <- function() {
        inv
    }
    
    # Return a list of the above functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # If the inverse is already cached, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    
    inv  # Return the calculated inverse
}

# Create a matrix
mat <- matrix(c(1, 2, 3, 4), 2, 2)

# Create the special matrix object
cachedMatrix <- makeCacheMatrix(mat)

# Compute the inverse (this will calculate and cache the inverse)
inverse1 <- cacheSolve(cachedMatrix)

# Compute the inverse again (this will retrieve the cached inverse)
inverse2 <- cacheSolve(cachedMatrix)
