## These functions are used to store a matrix and cache the inverse
## The first function stores a list containing four functions
## The functions store and set the values of x and inv 
## x is the matrix, inv is the inverse of the matrix
## The values of x and inv persist in memory because the 
## the functions get, set, setinverse and getinverse were created
## within the makeCacheMatrix function so any reference to these 
## variables by the functions within that scope will retrieve
## the values stored.


## This first function returns a list containing four functions:
##	1. set - sets the value of x internally and sets inv to NULL
##	2. get - returns the value of x
##	3. setinverse - stores the value passed to it in inv
##	4. getinverse - returns the value of inv
## setinverse and getinverse (generally) should not be called directly
## These two functions are called by the cacheSolve function below

## Usage example:
## > mymatrix <- makeCacheMatrix(rbind(c(4,7), c(2,6)))
## > mymatrix$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6


makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) inv <<- solve
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve checks if the inverse of a matrix is cached using getinverse.
## If the inverse is cached, the value is returned without recalculating

## Otherwise, the solve function calculates the inverse of the matrix
## The results is passed to the setinverse function to cache it
## Then the calculated inverse of the matrix is returned 

## Usage example: 
## > cacheSolve(mymatrix)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		    inv <- x$getinverse()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
}

## Validation:
## > mymatrix$get() %*% cacheSolve(mymatrix)
## getting cached data
##     [,1] [,2]
## [1,]    1    0
## [2,]    0    1
