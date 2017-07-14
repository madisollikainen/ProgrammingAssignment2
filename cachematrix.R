## Coursera JHU R Programming course week 3 homework
##
## Author: Madis Ollikainen
## 
## Description: This code implements helper functions for caching the results of
##              matrix invers calculations. Function makeCacheMatrix generates
##              a special 'cacheble matrix' object, which consist of a list of  
##              of functions: getter and setters for a matrix and its invers.   
##              The cacheSolve function is a simple wrapper for the R solve
##              function, which uses the invers caching capability of the 
##              'cachable matrices' generated with makeCachMatrix.
##
## Note: The functions below expect the user to co-operate and provide an 
##       invertible matrix as an input to makeCacheMatrix and a cacheble matrix
##       object as an input to cacheSolve function. Currently there are not 
##       control checks implemented to ensure that the inputs are correct. 
##
## Note 2 : The code is heavily based on the vector mean caching example made 
##          while explaning the problem premises. 



## makeCacheMatrix ::   generates a 'cacheble matrix' object; i.e a list of 
##                      getter and setter for a matrix and its invers. 
##
## Input x: An invertible matrix 'x', e.g matrix(1:4, nrow=2, ncol=2)
##
## Output out: A list 'out' of getters and setters for the matrix and its invers
##      
#       out$get()        - returns the matrix x
##      out$set(y)       - sets x=y and clear the invers value (inv=NULL)
##      out$getInvers()  - returns the inv variable, which is either the invers
##                         of x or NULL (if the invers hasn't been set yet)
##      out$setInvers(y) - set inv=y, i.e caches y as the invers of x; it is not 
##                         checked whether y actually is the invers of x.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInvers <- function(inverse) inv <<- inverse
    getInvers <- function() inv
    list(set=set, get=get,
         setInvers=setInvers, getInvers=getInvers)
}


## cacheSolve :: Wrapper around R solve function; alows matrix invers caching.
##
## Input x: A 'cacheble matrix' object created with makeCacheMatrix, where the 
##          matrix is invertible.
##
## Output : The invers of the matrix in the 'cacheble matrix' object. 
##
## Method: If the 'cacheble matrix' object has a cached invers, then this is 
##         returned, otherwise the R solve function is used to calculate the
##         invers; if the invers is calculated, it is also stored in the 
##         'cacheble matrix' object. 
cacheSolve <- function(x, ...) {
    inv <- x$getInvers()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInvers(inv)
    return(inv)
}


