## Functions to cache the inverse of a matix

## makeCacheMatrix creates a matrix object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ##Initialize the inverse           
        i <- NULL
        
        ## Method to set the matrix
        set <- function (matrix)
        {
                m <<- matrix
                i <<- NULL
        }
        
        ## Method to get the matrix
        get <- function(matrix){
                m
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse){
                i <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function(){ 
                i 
        }
        
        ## Returns a list of methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve computes the inverse of the matrix returned
## by "makeCacheMatrix" function. If the inverse is already
## calculated, then the below function will return the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return the inverse, if its already set
        if ( !is.null(m) ){
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()

        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data

        ## Set the inverse to the object        
        x$setInverse(m)
        
        ## Return the matrix
        m
}
