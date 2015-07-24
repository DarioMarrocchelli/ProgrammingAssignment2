## The functions written here allow the user to set a matrix, read it and 
## calculate its mean. The mean of the matrix gets cached, so that one does 
# not need to calculate it everytime.

## At the end of this file, I report some instructions detailing how 
## one can run these functions

## This function stores 4 functions. These are set, get, setSolve and getSolve
## set() : sets a new matrix
## get() : returns the value of 'x'
## setSolve() : stores the input in a variable m into the main function 
## getSolve() : returns the variable m

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve = matrix()) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function returns a matrix that is the inverse of 'x'
## Since this calculation can be very expensive, the function checks if this
## has already been calculated, in which case it simply returns the cached 
## value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## The if statement checks whether the inverse of this matrix has already
    ## been calculated (and the matrix has not changed). If so, it returns the
    ## cached  value 'm'
    
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## This part calculates the inverse of 'x'
    
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}


## How to run these functions
## matrix <- matrix(runif(1000000,0,1),1000,1000) :: generates a 1000x1000
## matrix with randon numbers
## a <- makeCacheMatrix(matrix) :: assigns the makeCacheMatrix to a (using the)
## matrix we definied above
## a$get(), a$set(), a$setSolve(), a$getSolve() :: these call the 4 individual
## functions (see explanation above)
## cacheSolve(a) :: calculates the inverse matrix; try calling this function a
## bunch of times; the first time it should be slow (it is calculating the
## inverse), but then it should be quicker
## Now try these commands: 
## matrix2 <- matrix(runif(1000000,0,2),1000,1000)
## a <- makeCacheMatrix(matrix2)
## cacheSolve(a)
## Why is it slow again?




