## Two functions are defined. First, a list variable is created from raw data with makeCacheMatrix()
## Then, the inverse matrix can be calculated / retrieved from the cache by cacheSolve()


## The function makeChacheMatrix() creates a list-variable from raw data. 
## inverse is the stored / cached inverse matrix from this data. It is set to NULL, when it has not been calculated yet or the
## raw data is altered by set(). The raw data can be seen with the get()-function.
## The inverse is set by setinverse(), which uses solve()
## The stored inverse matrix is returned by getinverse()

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function cacheSolve() returns the inverse of the matrix. The input is a pre-calculated variable from makeCacheMatrix.
## If there is no chached data in this variable (via getinverse()), then function gets the raw data, the inverse is calculated by
## solve() and set into the cache (setinverse()). Finally, the inverse is returned (either cached with comment or newly calculated)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                 inverse <- x$getinverse()
         if(!is.null(inverse)) {
                 message("getting cached data")
                 return(inverse)
         }
         data <- x$get()
         inverse <- solve(data, ...)
         x$setinverse(inverse)
         inverse
}
