##alvtrak
##Coursera rprog-013
##24/April/2015
##Assigment 2


## "This function creates a special "matrix" object that can cache its inverse
## I have used the example given for mean and make changes to cater for inverseMatrix
## where the inverseMatrix of X is stored
makeCacheMatrix <- function(x = matrix()) {

        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(x) im <<- x
        getInverseMatrix <- function() im
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        im <- x$getInverseMatrix()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        ##calculate the inverse matrix
        im <-solve(data) %*% data
        x$setInverseMatrix(im)
        im        
}
