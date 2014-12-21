## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix stores and access the input square matrix and 
##	its calculated inverse. 
## cacheSolve returns inverse of the square matrix inputed in 
##	makeCacheMatrix function.  The inverse of the matrix is
##	calculated if it did not exist before.  If the inverse
##	of the matrix has been calculated before, it will access
##	the value from memory.  By accessing the already-calculated
##	inverse matrix, it will save computation time and resourse.


## Write a short comment describing this function
##
## This function stores and access the input square matrix and its
##	calculated inverse one. 

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL			        # resets inverse matrix to NULL
	get <- function() x		        # assigns input matrix
	setinv <- function(solve) {	        # stores inverse matrix by
		s <<- solve		        # 	super-assignment / add to
	}				        #	the inverse matrix list in
					        #	different level
	getinv <- function() s		        # access inverse matrix from
					        #	cache/memory
	list(get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##
## This function returns the inverse of the square matrix by 
##	calculation or accessing the inverse matrix from the memory
##	which comes from previous calculation.

cacheSolve <- function(x, ...) {
	s <- x$getinv()			        # accessing the input matrix
	if(!is.null(s)) {			# checking if the inverse matrix
						#	exist in the memory
		message ("getting cached data")
						# print the message if the 
						#	inverse exists in the
						#	memory
		return(s)			# return the inverse matrix from
						#	memory
	}
	data <- x$get()			        # get the input matrix since
						#	its inverse does not exist
	s <- solve(data, ...)		        # calculates input matrix's
						#	inverse
	x$setinv(s)				# write the result in the memory
	s					# return the inverse matrix
}
