## Note1: Assume that the matrix supplied is always invertible!!!

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        setM <- function(y) {
                x <<- y
                m <<- NULL
        }
        getM <- function() x
        setMinv <- function(solve) m <<- solve
        getMinv <- function() m
        list(setM = setM, getM = getM,
             setMinv = setMinv,
             getMinv = getMinv)
}

 ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
 ## If the inverse has already been calculated (and the matrix has not changed),
 ## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
                m <- x$getMinv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$getM()
                m <- solve(data, ...)
                x$setMinv(m)
                m
}

##Prove

a<-  makeCacheMatrix(matrix(c(1,3,2,7),2,2))  
cacheSolve(a)

