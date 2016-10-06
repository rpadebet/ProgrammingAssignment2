makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


# Test code
set.seed(50)
test <- runif(20000000,0,1)  # create a large vector of random uniforms
V <- makeVector()  # create the list of 4 functions to handle the special vector
V$set(test)     # assign the vector test to the list of special functions
V$setmean(20)   # setting arbitrary mean to test output of cache call
head(V$get())  #verify to see if it was set properly
cachemean(V)  # should return 20 from cache
##  getting cached data
##  [1] 20
V$setmean(NULL) # setting mean back to null
cachemean(V)   # this should return the actual computed mean
##  0.4999124
V$setmean(NULL) # resetting mean to compute system.time

system.time(cachemean(V)) # gives actual time to compute
##      user  system elapsed 
##      0.043   0.000   0.043 
system.time(cachemean(V)) # gives the time to retrieve from cache
##      getting cached data
##      user  system elapsed 
##      0.001   0.000   0.000 
