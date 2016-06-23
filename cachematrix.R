## Overall, this function creates a list 
## containing functions called set and get which interfaces with a created matrix
## and setInverseMatrix and getInverseMatrix which manipulates the orginal matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setInverseMatrix <-function(solve) m <<- solve
        
        getInverseMatrix  <- function() m
        
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
        
}


## This functions calculates the inverse of the matrix created above by the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        
        ## Checks to see if the the inverse matrix has already been calculated, if so R pulls the data from the cache
        if (!is.null(m)) {
                
                message("Your data is being pulled from cache. Hopefully you are saving lots of time by doing this!")
                return (m)
        }
        
        data <-x$get()
        m <-solve(data, ...)
        x$setInverseMatrix(m)
        return(m)
}
