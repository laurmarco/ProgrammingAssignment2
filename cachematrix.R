## This pair of functions provides an efficient way to calculate the inverse of a matrix by 
## leveraging lexical scoping and the cache of a function environment.

## makeCacheMatrix
## First, the function objects x and z are initialized where x is a matrix and z is a variable to store the inverse of that matrix. 
## Then, it returns four functions: set() to mutate the values of x and z in the parent environment,
## get() to fetch the matrix x. setinverse() to assign the inverse of the matrix to z,
## and getinverse() to fetch the inverse z.
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


## cacheSolve
## Step one, use the getinverse() function to get the stored inverse from the input.
## Step two, check if the stored inverse is non-NULL. If it is, return it directly. 
## Step three, use the get() function to return the matrix and use solve() to get the inverse.
## Step four, set the new inverse and return it.

cacheSolve <- function(x, ...) {
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
