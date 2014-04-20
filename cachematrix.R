## The first function makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to 
## 1. set the value of the matrix itself
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix x
## 4. get the value of the inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
	inverse_of_x <- NULL
	
	# set the value of the matrix
	setMatrix <- function(y){
		x <<- y
		inverse_of_x <<- NULL
	}
	
	# get the value of the matrix: x
	getMatrix <- function(){
		x
	}
	
	# set the value of the inverse of matrix x, using the <<- operator to assign
	# a value inv_x to inverse_of_x which is different from the current environment
	setInverse <- function( inv_x ){
		inverse_of_x <<- inv_x
	}
	
	# get the value of the inverse of matrix x
	getInverse <- function(){
		inverse_of_x
	}
	
	# return a list containing four functions 
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getinverse)
}


## This function cacheSolve() can calculate the inverse of the special "matrix" with functions of makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## x is an object of the list above
		inverse_of_x <- x$getInverse()
		
		# check if the inverse of  'x' is NULL
		if( !is.null(inverse_of_x) ){
			# if not, return the result directly
			message("getting the inverse of the matrix x ~ ")
			return(inverse_of_x)
		}
		
		# if the inverse of x is NULL, we need to calculate the inverse matrix using the 'solve' function
		# firstly, get the original matrix
		data <- x$getMaxtrix()
		
		# calculate the inverse
		inverse_of_x = solve(data, ...)
		
		# set the inverse matrix calculated above currently to the inverse matrix of caching object 
		x$setInverse(inverse_of_x)
		
		# finally, return the result
		inverse_of_x
}
