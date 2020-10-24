## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ##initialize the inverse property
        i <- NULl
        
        ##Method to set the matrix
        set <- function(){
                ##Return the matrix
                m
        }
        
        ##Method to set the inverse of the matrix
        setInverse <- function(inverse){
                i<-inverse
        }
        
        ##Method to get the inverse of the matrix
        GetInverse <- function(){
                ##return the inverse property
                i
        }
        
        ##return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Just return the inverse if its already set
        if( !is.null(m) ) {
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
