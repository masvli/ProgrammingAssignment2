## Put comments here that give an overall description of what your
## functions do

# Author: Sven Mader
# Date: September 2014
# Description: 
# Both functions (makeCacheMatrix and cacheSolve) are part of the 2nd programming
# assesement for the r-programming course in coursera.
# The aim of the function set it to provide a extended matrix with get and set 
# functions to store larger matrices and their inverse plus to calculate the 
# inverse with use of the cached content.

## Write a short comment describing this function

# Function: makeCacheMatrix
# Parameter: x = matrix()
# Return value: a list object with functions for the stored matrix
makeCacheMatrix <- function(x = matrix()) {
	inv_mat <- NULL #set default value for inverse matrix variable
	
	# function to set content of x + check if matrix changed. 
	set <- function(new_mat = matrix()) {
		
		# check matrix, if identical to existing, no need for in_mat reset
		if (!identical(x, new_mat)) {
			inv_mat <<- NULL #new_mat and x differ, so inverse has to be re-calculated.
		}
		
		# set matrix
		x <<- new_mat
	}
	get <- function() x
	
	# set & get inverse functions
	setinverse <- function(solve) inv_mat <<- solve
	getinverse <- function() inv_mat
	
	# store for functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# Function: cacheSolve
# Parameter: x as a matrix created with makeCacheMatrix
# Return value: inverse of the matrix of x. Either calculated from scratch if 
# it does not yet exist or returend from cache, if already calculated.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    
    # get inverse of matrix
    inv <- x$getinverse()
        
    # check if inverse is NULL, if not, return inverse
    if(!is.null(inv)){
    	message("... get chached data ...")
    	return(inv)
    }
        
    # inverse is currently null, so get matrix data 
    data <- x$get()
        
    # and start solve function
    inv <- solve(data, ...)
        
    # update store
    x$setinverse(inv)
        
    #return inversed matrix
    inv
}
