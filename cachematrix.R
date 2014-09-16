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
	setinverse <- function(solve) inv_mat <<- solve
	getinverse <- function() inv_mat
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("... get chached data ...")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmean(inv)
        inv
}
