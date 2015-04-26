## With these functions you can create a cache for the
## inverse calculation of a matrix.

## This creates a list with four functions,
## to set/get the value of the matrix,
## and to set/get the inveres of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL

    # create set/get functions
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i

    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

## This calculates the inverse of a matrix, if it is already calculated,
## it returnes the inverse from the cache without calculation
cacheSolve <- function(x, ...) {

    i <- x$getmatrix()
    
    # check if inverse is already calculated
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()

    # calculate the inverse
    i <- solve(data, ...)
    
    # save the inverse
    x$setinverse(i)
    
    # return the inverse
    i
}
