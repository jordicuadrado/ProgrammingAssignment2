##  Caching the Inverse of a Matrix.
## This file contains a pair of functions that cache the inverse of a matrix,
## as it is usually a costly computation one may want to avoid.
##
## For further details, please see the Coursera R Programming assignment:
## https://github.com/rdpeng/ProgrammingAssignment2


## makeCacheMatrix defines the getters and setters for the values of the matrix and the inverse matrix.
##
## Use example:
## x = rbind(c(-1/2, 1), c(1, -1/2))
## mat = makeCacheMatrix(x)
## mat$get()  #gets the matrix
##
## cacheSolve(mat) #computes and caches the data (as it's the first run)
## cacheSolve(mat) #gets the cached data (subsequent runs)
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {  #setter for the matrix x
        x <<- y
        invMat <<- NULL
    }
    get <- function() x   #getter for the matrix x
    setInverse <- function(inverse) invMat <<- inverse   #setter for the inverse matrix
    getInverse <- function() invMat   #getter for the inverse matrix
    list(set=set, get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}


## cacheSolve returns the inverse of the matrix x.
## Prerequisite: matrix x is always invertible.
## If the matrix has already been computed returns the cached result.
## Computes the inverse matrix otherwise.
##
## Use example (see usage of function makeCacheMatrix above):
## cacheSolve(mat) #computes and caches the data (as it's the first run)
##          [,1]      [,2]
##[1,] 0.6666667 1.3333333
##[2,] 1.3333333 0.6666667
## cacheSolve(mat) #gets the cached data (subsequent runs)
##[1] "lucky you! inverted matrix already computed, getting the cached data"
##          [,1]      [,2]
##[1,] 0.6666667 1.3333333
##[2,] 1.3333333 0.6666667
cacheSolve <- function(x, ...) {
    invMat <- x$getInverse()
    if(!is.null(invMat)) {
        print("lucky you! inverted matrix already computed, getting the cached data")
        return(invMat)
    }
    dataMat <- x$get()
    invMat <- solve(dataMat)
    x$setInverse(invMat)
    invMat
}