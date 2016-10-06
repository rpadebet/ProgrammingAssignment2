## cacheSolve is used to return the inverse of a special matrix assuming it is square and invertible
## It speeds up the computation time by getting the value from cache if the inverse of the matrix is already calculate

## the makeCacheMatrix is used to get and set a matrix and its inverse in form of a list

## This function is used to get/set a matrix and its inverse as a list

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function returns the inverse of a matrix. It returns the value from cache if it is
## already calculated and exits, else it computes the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    invisible(m) # done to supress the output in case of large matrices
}

# Test code
set.seed(50)
n<-500
testMatrix <- matrix(runif(n^2,0,1),n,n)  # create a large matrix of random uniforms
M <- makeCacheMatrix()  # create the list of 4 functions to handle the special matrix
M$set(testMatrix)     # assign the matrix testMatrix to the list of special functions
M$setinv(matrix(rep(1,n^2),n,n))   # setting a nXn matrix of 1's to test output of cache call
# head(M$get())  #verify to see if it was set properly
testones <- cacheSolve(M)  # should return the nXn matrix of 1's from cache
##  getting cached data
head(testones)
M$setinv(NULL) # setting inverse matrix back to null
actual <- cacheSolve(M)   # this should return the actual computed inverse
##  
head(actual)
M$setinv(NULL) # resetting inverse matrix back to compute system.time

system.time(cacheSolve(M)) # gives actual time to compute
##      user  system elapsed 
##      0.241   0.000   0.241 
system.time(cacheSolve(M)) # gives the time to retrieve from cache
##      getting cached data
##      user  system elapsed 
##      0.001   0.000   0.000 