## makeCacheMatrix will take a matrix and create a list of 
## functions to be passed to other functions later on,
## allowing for the storage of a given matrix or setting
## a new matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  ## Initializes 'm' in preparation for cacheSolve().
    
    set <- function(y) { ## Allows the matrix to be reset, and empties
        ## the cached value of 'm'.
        x <<- matrix(y)
        m <<- NULL
    }    
    get <- function() x     ## Retrieves the matrix passed to makeCacheMatrix
    setinverse <- function(inverse) m <<- inverse   ## Will set 'm' as inverse globally   
    getinverse <- function() m      ## Retrieves inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    ## Returns a list of the four functions to be passed to the next function.
    
}

## cacheSolve will take the result of makeCacheMatrix - a list of functions and a matrix -  
## compute the inverse of the matrix, and store that in 'm'.  If 'm' has already been solved/filled, 
## cacheSolve will print a message and return it.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()     ## Checks to see 'm' has been stored and attempt to return.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Computes the inverse of the original matrix and stores it in 'm', while calling the setinverse
    ## function in the makeCacheMatrix object to set 'm' to the inverse matrix, globally.
    data <- x$get()  
    m <- solve(data, ...)
    x$setinverse(m)
    m
}