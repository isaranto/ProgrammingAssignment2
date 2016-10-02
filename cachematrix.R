## This is the second assignment for the coursera course
## R Programming
## Ilias Sarantopoulos
## Write a short comment describing this function
## We create a "makeCacheMatrix" object with which we can get/set the inital data(the matrix)
## and also get/set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y=matrix()) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse=matrix()) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## With this function (as done with the mean in the example) we try to get the inverse
## if it has already been calculated, then it exists in the object-memory. Otherwise we
## calculate it and we set it using the setinverse() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
