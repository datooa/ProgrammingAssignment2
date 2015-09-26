## These functions allow for matrices and their solutions (inverses)
## to be stored within a list of functions ('makeCacheMatrix') and 
## called upon with a different function ('cacheSolve') that can 
## find the inverse of the matrix input, or simply return the 
## cached value if it already exists

## Creates a list of four functions related to matrix 'x' and its 
## inverse. The functions in this list can be called later as 
## subsets of makeCacheMatrix (after it is assigned to an object) 
## using the cacheSolve funtion. Crucially, the 'set' function 
## allows the input matrix and its solution (from the larger 
## containing function) to be reset

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv 
    getinv <- function() inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the cached inverse if it exists in the list. Otherwise, 
## find the inverse and store the solution

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if (!is.null(inverse)){
        message("returning cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
    ## Return a matrix that is the inverse of 'x'
}
