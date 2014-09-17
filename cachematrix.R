## Here are defined two functions which help us store and cache the inverse of a matrix with the matrix.


## makeCacheMatrix returns a list of functions which help us interact with the matrix. We can set and get both the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()){
    # Initialize inverse variable.
    inverse <- NULL
    
    # Define set function which initialize the matrix. Inverse is defaulted to not calculated.
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    # Define get function which returns the matrix.
    get <- function() x
    
    # Define setinverse function which assigns a value to the inverse variable.
    setinverse <- function(z) inverse <<- z
    
    # Define getinverse function which returns the inverse.
    getinverse <- function() inverse
    
    # Returns the list of all functions.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve set and cache the inverse of a matrix - if the matrix already has an inverse, cacheSolve will return this inverse.
cacheSolve <- function(x, ...){
    # Retrieve existing inverse of x
    s <- x$getinverse()
    
    # If the inverse exists, return the inverse with a message
    if(!is.null(s)){
        message("getting cached inverse")
        return(s)
    }
    
    # Retrieve the matrix
    data <- x$get()
    
    # Calculate the inverse
    s <- solve(data,...)
    
    # Set and cache the inverse into x
    x$setinverse(s)
    
    # Return the inverse
    s
}
