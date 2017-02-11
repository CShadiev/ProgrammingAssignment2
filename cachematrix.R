## This version of the code is a lazy replication
## of the given example with minor changes to 
## get the desired result.

## Unfortunately, for some reason the way suggested 
## in the example didn't work for me as expected,
## therefore I changed one important factor:
## I have created hidden object that stores the 
## result of a computation - ".invMatrix".
## There are some other minor changes:
## The argument type for "makeCacheMatrix",x, is 
## changed to "matrix()"; .invMatrix" is used
## instead of "m". Also, "setInverse" and "getInverse"
## are equivalent of "setmean" and "getmean" in the ex.


makeCacheMatrix <- function(x=matrix()) {
    .invMatrix <<- NULL
    set <- function(y) {
        x <<- y
        .invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) .invMatrix <<- inverse
    getInverse <- function() .invMatrix
    list(set=set,get=get,setInverse=setInverse,
         getInverse=getInverse)
}


## This function is very close to the one given in the
## example. In my version it stores results in the 
## hidden object ".invMatrix", though and respectively
## it is where it check if there is something already
## stored. Everything else is pretty straightforward.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- .invMatrix
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <-solve(data, ...)
    x$setInverse(invMatrix)
    invMatrix
}
