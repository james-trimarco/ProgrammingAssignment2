## Together, the functions makeCacheMatrix and cacheSolve find the 
## inverse of a matrix, but only after checking to see if the 
## inverse has already been found. 

## The function makeCacheMatrix creates a special variable, i, and sets its 
## value to NULL. This function also creates four closures, which are used
## by the next function to find the inverse. Finally, it creates a list with
## all four closures in it.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve finds the inverse of a matrix, after that matrix 
## has been passed to makeCacheMatrix. cacheSolve checks to see if x$getinverse
## is empty. If it is, then it runs the solve function and then stores the result 
## in x$setinverse. If i is not empty, then cacheSolve will return the cached 
## inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
