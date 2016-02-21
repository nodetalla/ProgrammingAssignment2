## cachematrix function
## two functions that are used to create a matrix, calculate it's inverse
## before calculating the inverse, the functions check if the inverse is already calculated
## if the inverse is calculated and the matrix did not change the inverse is cached 
## from the memory instead of calculating it again

## function-1 makecacheMatrix
##uses an input matrix to create a list that contains the matrix and it's inverse matrix 
##the function can be used to create the matrix and calculate it's inverse matrix 
##or cache its previously calculated values from memory

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function-2 cacheSolve
##checks if the inverse matrix was calculated before and gets it from the cache
## When loaded from memory, the function displays a message to inform the user
## if the inverse matrix is not available it calculates and returns it.

cacheSolve <- function(x, ...) {
        #check if the inverse matrix exists in memory       
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                # if the inverse is available cache it and exit without calculations
                message("Invers matrix available in cache memory, loading cached data...")
                return(inv)
        }
        
        #if the inverse matrix is not available solve for it and return it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
