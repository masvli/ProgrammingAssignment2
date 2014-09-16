## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
