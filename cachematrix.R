## The following two functions help to optimize calculations of matrix
## inversions by a priori determining if the inverse of a customized matrix 
## object has already been cached.  If so, the inverse of the matrix will not 
## be recalculated, but will be retrieved from the cache, significantly
## reducing the computation time.  Otherwise, the matrix inverse will be 
## calculated normally.

## modified by: T.P.

## last update: June 20, 2015

## A function to create a matrix object that has the ability to cache its
## own inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set matrix i (a placeholder for the inverse matrix) to NULL:
    i <- NULL
    ## create a function to allow one to set the value of
    ## x after the fact, or change its value:
    setmatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## create a function to retrieve the matrix x:
    getmatrix <- function() x
    ## create a function to set the inverse i of matrix x:
    setinverse <- function(inverse) i <<- inverse
    ## create a function to get inverse i of matrix x:
    getinverse <- function() i
    ## return a list of the above four functions:
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse, getinverse = getinverse)

}


## A function to determine the inverse of the matrix object produced by
## the function makeCacheMatrix.  To improve computation times, cacheSolve 
## will retrieve this inverse from the cache if this calculation was done 
## previously.

cacheSolve <- function(x, ...) {
    ## retrieve inverse from list x and set result to i:
    i <- x$getinverse()
    ## test if i is not NULL; if TRUE then retrieve inverse
    ## from the cache and return its value:
    if(!is.null(i)) {
        message("retrieving inverse from cached data")
        return(i)
    }
    ## retrieve matrix from list x and set result to m:
    m <- x$getmatrix()
    ## calculate inverse of m using function solve():
    i <- solve(m, ...)
    ## now set the value of inverse i in list x:
    x$setinverse(i)
    ## return i
    i
}
