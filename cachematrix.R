## These functions take advantage of R's lexical scoping to retrieve calculated
## values stored in a special 'matrix object'.
## This 'matrix object', or 'makeCacheMatrix object', is created by
## makeCacheMatrix()

## Since the output of makeCacheMatrix() contains functions, a 'makeCacheMatrix 
## object' will store the entire environment of makeCacheMatrix()
## this includes the values of 'x', the initial input matrix, and 'i',
## its inverse

## cacheSolve() can access these values with the functions provided to it by
## makeCacheMatrix()


## makeCacheMatrix() creates a special 'matrix object' that can store the values
## of the initial input matrix, 'x', and its inverse, 'i'.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() is able to use the functions provided to it by makeCacheMatrix()
## to either calculate the inverse of matrix 'x', or retrieve the value of the
## inverse that's stored/cached in the 'makeCacheMatrix object'.

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## the 'makeCacheMatrix object' is called 'a' here because it's easier
        ## for me to understand 
        
        i <- a$getinverse()
        
        if(!is.null(i)) {
                
                message("getting cached data")
                return(i)
        }
        
        data <- a$get()
        i <- solve(data, ...)
        a$setinverse(i)
        
        i
}
