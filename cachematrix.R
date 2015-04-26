## Put comments here that give an overall description of what your
## functions do

## Overall Description:
## These functions automatically calculate the inverse of a matrix


## Write a short comment describing this function:

## The makeCacheMatrix function takes as input a matrix and creates a
## special "vector", which is really a list containing functions to:
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {  # makeCacheMatrix function defined with a matrix as an argument
        table <- NULL                        # initializes a matrix with no values
        set <- function(y) {                 # set function defined, it changes the input of function makeCacheMatrix
                x <<- y                      # sets input of makeCacheMatrix x to the new value y
                table <<- NULL
        }
        get <- function() x                                 # function that returns the matrix
        setinverse <- function(inverse) table <<- inverse   # function that sets the value of the table to be the inverted matrix
        getinverse <- function() table                      # function that returns the inverted matrix
        list(set = set, get = get,                          # a list of the items 1), 2), 3), 4) mentioned above is returned
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function:

## The cacheSolve function calculates the inverse of the matrix that was the
## input of the special "vector" created with the makeCacheMatrix function above,
## but only if the inverse matrix hasn't already been calculated and cached.
## If it has been calculated, it retrieves it from the cache.
## If it hasn't been calculated, it calculates it and stores it in the cache.

cacheSolve <- function(x, ...) {                        # cacheSolve function defined with a matrix as an argument, that returns the (possibly cached) inverted matrix  
        ## Return a matrix that is the inverse of 'x'
        table <- x$getinverse()                         # the value of table is taken by the getinverse object of makeCacheMatrix function
        if(!is.null(table)) {                           # if the matrix is not an empty matrix (i.e. it has been calculated before and been cached), ... 
                message("getting cached data")
                return(table)                           # ...it returns the matrix, which is the CACHED inverted matrix
        }
        data <- x$get()                                 # ...or else, 
        table <- solve(data, ...)                       # it CALCULATES the inverted matrix from the data retrieved from the makeCacheMatrix function,
        x$setinverse(table)                             # caches the newly inverted matrix
        table                                           # and returns the inverted matrix itself
}

