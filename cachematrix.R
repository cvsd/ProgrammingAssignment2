## Programming assignment 2 of R Programming requires an R function that caches
## matrix inverse previously computed, since the computation is potentially
## time-consuming. This will be done by two functions, a constructor and a
## computor.

## The function makeCacheMatrix() constructs a matrix with cached inverse. It
## has four sub-functions:
## set() sets the matrix with input data;
## get() returns the cached matrix;
## setinverse() computes the inverse of the cached matrix;
## getinverse() returns the cached inverse.

makeCacheMatrix <- function(m = matrix()){
    ## inv is the inverse of matrix; initialized with NULL
    inv <- NULL
    
    ## the function set() constructs a matrix with cache from the input m and
    ## initializes its inverse with NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    
    ## the function get() returns matrix data
    get <- function() m
    
    ## the funtion setinverse() sets inverse of the matrix m givien in parent
    ## environment using the function solve() that solves the equation 
    ## a %*% x = b for x, where b can be either a vector or a matrix
    ## inv is the variable of parent environment, so the operator <<- is used
    setinverse <- function() inv <<- solve(m)
    
    ## the function getinverse() returns inverse data
    getinverse <- function() inv
    
    ## the parent function makeCacheMatrix() returns a list of 4 elements
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve() computes the inverse of a matrix with cache. If 
## the matrix has cached inverse, the cached data is returned; otherwise it
## will be computed and cached, then returned.

cacheSolve <- function(m, ...){
    ## inv is the cached inverse of the matrix
    ## 
    inv <- m$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }else{
        message("no cached inverse; computing")
        m$setinverse()
        inv <- m$getinverse()
    }
    inv
}
