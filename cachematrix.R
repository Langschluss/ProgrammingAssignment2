## OK, so we all know the example code. Mine is basically the same,
## all I did was replacing the appropriate "stuff" with solve and so on.
## So in principle the functions should work correctly. 
## Briefly: makeCacheMatrix takes a square matrix as input.
## this matrix has to be assigned to a variable BEFORE giving
## it to the function. 
## makeCacheMatrix(1:4,2,2) doesn't work. However, generating
## the matrix before (e.g.: x <- matrix(1:4,2,2)) and giving then
## x to makeCacheMatrix works. If this is then assigned to another
## variable (e.g.: y <- makeCacheMatrix(x)), the inverted matrix
## of x is calculated and stored in the cache. If x is not changed and
## cacheSolve is executed again, it will get the inverted matrix from the 
## cache. If x is changed (and makeCacheMatrix(x) is executed), the
## inverted matrix will calculated again.

## makeCacheMatrix stores a matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
      
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
## this block basically clears the cache if a matrix had already been 
## stored before.
      
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve calculates the inverted matrix of the cached on. If the cached
## matrix had already been solved before with cacheSolve, it will retrive
## the already solved but still cached Matrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
      
    if(!is.null(m)) {                     
        message("getting cached data")
        return(m)
    } 
     
## this if block before is the key thing in retrieving the already 
## solved inverted matrix which is stored in the cache. 
## !is.null(m) means, that if m is NOT NULL, the block would be TRUE and
## the inverted matrix has already been stored in m. Therefore this code
## is executed and retrieves the inverted matrix from the cache.
## If m is NULL - this if block would be FALSE and therefore not executed.
      
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
} 

## In this last block, the inverted matrix is calculated. 
## The value of the inverted matrix is stored in the cache via the `setmean`
## function.
  
