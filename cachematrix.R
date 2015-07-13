
## My functions  inverse of a matrix and then cached it

## This function creates a special "matrix" object that can cache its inverse.
## This function containing the folowing:
##set the value of the matrix 
##get the value of the matrix
##set the value of inverse of the matrix
##get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## The next variable "i" is updated variable which show us that the 
    ## inverse of a matrix computes (return "!NULL") or not (return "NULL")
    i <- NULL
    ## et the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ##get the value of the matrix
    get <- function() x
    ##set the value of inverse of the matrix
    setinv <- function(inv) i <<- inv
    ##get the value of inverse of the matrix
    getinv <- function() i
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    ##test the meaning of previous function if   
    ##the inverse has already been calculated
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## calculate the inverse of matrix
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    ##Return a the meaning of calculation
    i
        
}
