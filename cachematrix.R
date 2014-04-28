## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
    # Sets x to be y
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # the get function just returns x if the set function has been run.  Otherwise it returns an empty variable
    get <- function() x
    
    # the setsolve function sets m to be the function solve
    setsolve <- function(solve) m <<- solve
    
    # getmean just returns the value of m.  if setmean has been called then it is mean, otherwise it is NULL
    getsolve <- function() m
    
    # list returns the set of functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         # define a function called x as the makeVector function.  This x is defined in this fuction not in the makeVector function
    x  <- makeMatrix()
    
    # Now set m as the matrix if it exists.
    m <- x$getsolve()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Set the variable data to be the output of the get function - this should be the matrix  
    data <- x$get()
    # Now set m to be the inverse of the matrix
    m <- solve(data, ...)
    # Now set the global environment m to be the same as the local environment m
    x$setsolve(m)
    # Return m.
    m
}
