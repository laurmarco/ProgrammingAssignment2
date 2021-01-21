## Put comments here that give an overall description of what your
## functions do

## First, the function objects x and z are initialized where x is a matrix and z is the inverse of that matrix. 
## Then, it returns four functions: set() to mutate the values in the objects in the parent environment,
## get() to define the getter for matrix x. setinverse() defines the setter for the inverse of the matrix
## that we store as z, and getinverse() that defines the getter for the inverse.
## Next, it creates and returns a list with named elements that are the four functions we defined.

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) z <<- inverse 
        getinverse <- function() z
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Write a short comment describing this function
## 

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}
