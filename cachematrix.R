## Put comments here that give an overall description of what your
## functions do
##
## Getting the inverse of a matrix is an expensive operation and if they
## will be use multiple times, it is optimum to cache them. The functions
## makeCacheMatrix and cacheSolve will make it possible. Thru the power of
## lexical scoping, makeCacheMatrix function accepts a matrix and returns
## a list of functions to set/get the matrix and to set/get the inverse. The 
## cacheSolve function checks if the inverse is in the cache and returns it, 
## or calculates the inverse, saves it in the cache and then returns it.

## To test in R,
##
## mat <- sample(c(1:36)); dim(mat) <- c(6,6)
## mat_cache_funs <- makeCacheMatrix(mat)
## cacheSolve(mat_cache_funs)
## cacheSolve(mat_cache_funs)
## round(cacheSolve(mat_cache_funs) %*% mat, 3)
##
## The first cacheSolve() should print the inverse if successfull.
## The second call to cacheSolve() will display "getting cached data" and
## returns the inverse matrix. The inverse multiplied by original matrix
## (%*% for matrix multiplication) should yield an identity matrix.


## Write a short comment describing this function

## makeCacheMatrix - creates a list of functions that set/get the
##                   a given matrix for the inverse to be solved and 
##                   setinverse/getinverse that set and get the inverse 
##                   of the matrix.
##
 
makeCacheMatrix <- function(x = matrix()) {
        my_inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) my_inv <<- inverse
        getinverse <- function() my_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##      cacheSolve - returns the inverse of matrix, first by checkng
##                   if the inverse of the matrix is in the cache and
##                   then returns it. If the inverse is not in the cache,
##                   call the solve function to get inverse of the matrix,
##                   saves it to cache and returns it. If the matrix is not
##                   defined such as null or not a square matrix or it is
##                   singular, R will return an error.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inv <- x$getinverse()
        if(!is.null(my_inv)) {
                message("getting cached data")
                return(my_inv)
        }
        data <- x$get()
        my_inv <- solve(data, ...)
        x$setinverse(my_inv)
        my_inv
}

