## makeCacheMatrix and cacheSolve allow to access the cache to retrieve the inverse of a matrix 
## if this has been previously calculated. If the inverse is not in the cache, this gets generated using
## the solve function and gets stored in the cache for easy access.

## makeCacheMatrix creates a list that contains the function that #1-sets the matrix, #2-gets the matrix 
## and then #3 sets the inverse of that matrix and #4 gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        #set i to NULL
        i <- NULL 
        # sets x to the argument y and i to NULL
        set <- function(y) { 
                x <<- y
                i <<- NULL
        }
        # return the value of the argument of makeCacheMatrix, x
        get <- function() x
        # set i to the inverve (x of makeCacheMatrix)
        setinv <- function(solve) 
                i <<- solve
        #return the value of i
        getinv <- function() i
        #return list of the values from the functions set, get, setinv amd getinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve checks to see if the inverse has already been calculated and is available in the cache
## and if it is the function gets the inverse from the cache; otherwise it calculates the inverse and sets 
## the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        # get the inverse from x if this was previously calculated (cache)
        i <- x$getinv()
        # if matrix is not empty, return i from the cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # if matrix is null, set the data to x from makeVector
        data <- x$get()
        # calculate the inverse 
        i <- solve(data, ...)
        # set i in x to the inverse
        x$setinv(i)
        # return the mean
        i        
}
