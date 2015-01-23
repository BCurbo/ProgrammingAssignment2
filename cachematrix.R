## These two functions will take a square matrix and calculate the
## inverse of that matrix. Both are stored as global variables
## this allows retrieval of the inverse of the matrix as long as 
## the matrix has not changed
## if the matrix changes, it will recalculate the inverse and hold 
## it globally.


## This function creates a global matrix and has 4 sub functions
##  1). set the matrix globally
##	2). Get the global matrix
##	3). get the inverse of the global matrix
## 	4). Set the inverse value of the global matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	setmatrix<-  function() {
		y <<- x
		i <<- NULL
	}

	getmatrix   <- function() x
	getinverse  <- function() i
	setinverse  <- function(solve) i <<- solve
	
	list(
	setmatrix	= setmatrix,
	getmatrix 	= getmatrix,
	getinverse 	= getinverse,
	setinverse  = setinverse
	)
}


## This function looks to see the global matrix has changed
## if it has not, it returns the global inverse.
## if it has changed, it calculates the inverse and places it globally
## and returns it


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
		## check to see if the matrix changed	        
        	l <- x$getmatrix()
        	sameMatrix <- identical(y,l)
        	if(sameMatrix) 
        	{
        			message("getting cached inverse")
        			i <- x$getinverse()
        			return(i)
        	}	
        data <-x$getmatrix()
    
        i <-  solve(data)
        x$setinverse(i)
        i
        
}


## Sample run:
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$getmatrix()
cacheSolve(m)
