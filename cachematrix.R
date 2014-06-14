## cachematrix.R

## This file contains functions two functions to create an object to 
## store a matrix and cache its inverse.



## makeCacheMatrix() -- function to create a list object to store a
## matrix object and store the inverse of the matrix.  We assume
## that any square matrix is invertible.  The function could be made 
## more robust by verifying that the supplied matrix object is square.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set<-function(y) {
        x <<- y
        I <<- NULL
    }
    get<-function() x
    setInverse<-function(inverse) I <<- inverse
    getInverse<-function() I
    list(set = set, get = get,
         setInverse = setInverse, getInverse=getInverse)
}

## cacheSolve() -- function to calculate the inverse of a matrix via
## the R solve() function.  To optimize speed, a cached version of the
## matrix inverse will be returned if it has been calculated previously.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
 I<-x$getInverse()
    if(!is.null(I)) {
        message("getting cached inverse")
        return(I)
    }
    data<-x$get()
    b<-solve(data)
    x$setInverse(b)
    b
}
