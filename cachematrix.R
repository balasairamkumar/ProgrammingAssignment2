## This R script has two functions makeCacheMatrix which create a special matrix object to 
## cache a inverse of input matrix

## This makeCacheMatrix function receives input a matix object and 
## initializes an object i to null which will hold the inverse of the input matrix
## functions set,get,setinverse,getinverse functions are for setting the input matrix to 
## an object x, getting the value of the matrix already stored, storing or caching the 
## inverse of input matrix in i (here the inverse value is received as parameter to the function),
## and returns the stored inverse matrix respectively. Finally a list has been created with 
## the above mentioned functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getmean <- function() i
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve function receives the special matrix that created by makeCacheMatrix as a parameter, 
## then it receives the cached inverse value and the matrix for which the cached inverse matrix is 
## calculated. 
## If the input matrix is same as old matrix then the cached inverse matrix is returned.
## the function identical is for checking whether two matrices are identical or not.
## if the input matrix is different from the old one, then this new matrix is cached and 
## its inverse is calculated using solve function and the inverse matrix also cached.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        y <- x$get()
        if(identical(x,y)){
                message("input is not get changed getting cached data")
                return(i)
        }
                
        y<-x$set(x)
        i <- solve(y, ...)
        x$setinverse(i)
        i
}
