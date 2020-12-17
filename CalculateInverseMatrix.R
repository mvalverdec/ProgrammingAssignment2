### Marco Valverde
### R_Programming
### Week 3, Assignment 2

# Set up the working directory on my laptop
setwd("D:/Proyectos/R/datasciencecoursera/ProgrammingAssignment2")


## We need a pair of functions that cache the inverse of a matrix
# Let's assume the matrix is inversible

## The next function, creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {    # it receives a matrix
        
        ## Initialize the inverse property
        i <- NULL
        
        ## Method to set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Method the get the matrix
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()    # Assings a var m with the inverse of the parameter
        
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