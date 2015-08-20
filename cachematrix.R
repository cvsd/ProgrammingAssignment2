## R function to cache matrix inverse computation

## The function makeCacheMatrix() constructs a matrix with its inverse cache:
## set() sets the matrix with input data and initialize its inverse as NULL
## get() returns the matrix
## setinverse() computes the matrix inverse
## getinverse() returns the cached matrix inverse

makeCacheMatrix <- function(m = matrix()){
    ## Default value of matrix inverse: NULL
    inv <- NULL
    
    ## set matrix with input data; initialize its inverse as NULL
    set <- function(x) {
        m <<- x
        inv <<- NULL
    }
    
    ## return matrix 
    get <- function() m
    
    ## compute matrix inverse using function solve()
    ## solve(a) solves the equation a %*% x = I (I is identity matrix)
    setinverse <- function() inv <<- solve(m)
    
    ## return matrix inverse
    getinverse <- function() inv
    
    ## R function makeCacheMatrix() returns a list of 4
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve() computes the matrix inverse. If the matrix 
## has cached inverse, then cached data is returned; otherwise the inverse
## will be computed and cached, then returned.

cacheSolve <- function(m, ...){
    ## inv is the cached inverse of the matrix that can be NULL
    inv <- m$getinverse()
    
    ## compute matrix inverse
    if(!is.null(inv)) {
    ## if inv is cached (a non-NULL value), then the cached data is 
    ## returned immediately
        message("getting cached inverse")
        return(inv)
    }else{
    ## if inv is NULL, then call function setinverse()
        message("no cached inverse; computing")
        m$setinverse()
        inv <- m$getinverse()
    }
    inv
}
