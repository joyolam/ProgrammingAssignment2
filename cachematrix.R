## makeCacheMatrix function that return a list that contains a matrix, its 
## inverse, and functions that set the original matrix, the inverse matrix, 
## and returns these matrices. 
## function cachesolve  computes and return the inverse of a matrix.
## if the inverse was previously computed, it will find the value in the cache
## and return it 


#function that return a special "matrix", that contain a matrix, its inverse and 
#"some" functions.
makeCacheMatrix <- function(x = matrix()) 
{
        #matrix to store the inverse
        z <- NULL
        #function set gives a value to the matrix X
        #the value of z is removed, since the new matrix may have a 
        #different value
        set <- function(y)
        {
                x <<- y
                z <<- NULL
        }
        #function get returns the value of matrix x
        get <- function() x
        #function setinv set value the inverse matrix z 
        setinv <- function(inv) z <<- inv
        #function getinv returns the inverse matrix z
        getinv <- function() z
        #special "matrix" returned by makeCacheMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#function that computes and return the inverse of a matrix.
#if the inverse was previously computed, it will find the value and return it 
cacheSolve <- function(x, ...) 
{
        #inverse matrix is obtained from x
        z <- x$getinv()
        #if different than null, retrieved from the cache
        #the return at the end of the if stops the execution
        if(!is.null(z)) 
        {
                message("getting cached data")
                return(z)
        }
        #if the value of z is null, the inverse is recalculated
        data <- x$get()
        z <- solve(data, ...)
        # the new value is reported to the special "matrix" x
        x$setinv(z)
        z
}
