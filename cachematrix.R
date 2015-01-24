## The following functions are able to cache time-consuming 
## computation of matrix inversion particularly in case of 
## large matrices.

## This function creates a special 'matrix' object that is
## basically a list to set the value of a matrix, get the
## value of the matrix, set the inverse of the matrix to another
## variable and get the inverse of the matrix.
## Example: matrixObject <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) { #matrix passed as arg here is the one whose inverse we want to save
    m <- matrix() #inverse matrix init
    set <- function(y) { #set the value of matrix using this function
        x <<- y #The <<- operator here sets the value of x in the parent function
        m <<- matrix() #m is set to NA matrix of dim = c(1,1)
    }
    get <- function() x #returns the value of matrix
    setinverse <- function(inv) m <<- inv #saves the value of a computed inverse to m
    getinverse <- function() m #retrieves the value of saved inverse from cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) #returns the list of functions
}

## This function returns the inverse of the given matrix and
## saves it into cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse() # Gets the current value of saved inverse.
    emp <- matrix(nrow=nrow(m),ncol=ncol(m)) #Initiates an empty matrix with the same dimensions as given matrix
    if(!identical(m,emp)) { #checks if the saved value of inverse matrix equal to NA matrix of the same size
        message("getting cached data")
        return(m) #returns the saved value of inverse
    }
    data <- x$get() #gets the given value of matrix
    m <- solve(data) #computes the inverse of the given matrix 
    x$setinverse(m) #saves the value of inverse of the matrix to memory
    m #inverse matrix is returned.
}