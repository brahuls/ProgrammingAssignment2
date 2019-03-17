################################################################################
## $Header:: "cachematrix.R"; version v 1.0.0 2019-03-17
## Author:: Rahul S. Brahmachari
##
## This contains two functions to illustrate the lexical scoping feature  
## of the R language; Assignment#02 on Week#03
## A pair of functions that cache the inverse of a square matrix to beneift 
## by eliminating the repeated costly computation
## We assume that the matrix supplied is a square matix and always invertible.
##
## The first function, makeCacheMatrix, creates a special "matrix" object, which 
## is really a list containing a function to 
##   a) set the value of the matrix, 
##   b) get the value of the matrix, 
##   c) set the value of the inverse of the matrix, and 
##   d) get the value of the inverse of the matrix
################################################################################
makeCacheMatrix <- function(x = matrix()) {
        m.inverse <- NULL
        set <- function(y) {
                x <<- y
                m.inverse <<- NULL
        }
        get <- function() { x }
        setInverse <- function(i) {m.inverse <<- i}
        getInverse <- function() {m.inverse}
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" returned 
## by  above written function makeCacheMatrix(). If the inverse has already been
## calculated  (and the matrix has not changed), it retrives the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of 
## the given matrix and sets the value of the inverse in the cache via the 
## setInverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {   ## If the inverse is aready cached
                message("getting cached data")
                return(inverse) ## Return cached matrix that's the inverse of 'x'
        }
        # If inverse has not yet been cached or the matrix has been changed, then
        data <- x$get()  # get the matrix 
        inverse <- solve(data, ...) # compute the inverse of the matrix
        x$setInverse(inverse)  # cache the inverse for future use
        return(inverse) # Return a matrix that is the inverse of 'x'
}

###############################################################################
## Sample testng the impact of cacheSolve for matrix inverse
## Setting up a test matrix 
## set.seed(3948)
## B <- matrix(rnorm(4000000), nrow=2000, ncol=2000)
## B1 <- makeCacheMatrix(B) ## Creating the special matrix object
## Creating a test functon
## testCacheInverse <- function(m1){
##         ## @m1 :: the special matrix object created by makeCacheMatrix()
##         m.start.time <- Sys.time()
##         cacheSolve(m1) ## Calling the cacheSolve to get the inverse        
##         print(Sys.time()-m.start.time) ## printing the processing time
## }
## > testCacheInverse(B1)
##   Time difference of 17.51965 secs
## > testCacheInverse(B1)
##   getting cached data
##   Time difference of 0 secs
## > testCacheInverse(B1)
##   getting cached data
##   Time difference of 0.0009999275 secs
## > testCacheInverse(B1)
##   getting cached data
##   Time difference of 0 secs
## > testCacheInverse(B1)
##   getting cached data
##   Time difference of 0 secs
## > testCacheInverse(B1)
##   getting cached data
##   Time difference of 0.0009999275 secs
################################################################################