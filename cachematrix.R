	makeCacheMatrix <- function(x = matrix()) {
	
	  invM <- NULL  #inverse matrix
	  
	  setMatrix <- function(y)
	    {
	       x <<- y #Stores y into the matrix "x" which is a variable created in another environment
	      
	      invM <<- NULL 
	    }
	    
	  getMatrix <- function() return(x) #Function that returns the matrix
	  
	  setInverse <- function(inverse)  invM <- inverse
	  
	  getInverse <- function() return(invM) #Function that returns the inverse matrix
	  
	  list(setMatrix = setMatrix,getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
	 	}
	 	

	 	
	 	## Write a short comment describing this function
	 	
	 	#CacheSolve retrieves a precomputed inverse of the matrix provided in the input.
	 	# If the inverse matrix has not already been computed, cacheSolve computes an inverse matrix
	 	# and stores in a variable for future use.
	 	
	 	cacheSolve <- function(x, ...) {
	 	        ## Return a matrix that is the inverse of 'x'
	  
	  invM <- x$getInverse
	  
	  #Conditional checks to see if the inverse was already calculated
	  
	  if(!is.null(invM))
	  {
	    message("getting cached data")
	    return(invM)
	  }
	  
	  #Calculate the inverse matrix (since it was not calculated and cached )
	  
	  inputMat <- x$getMatrix() # inputMat stores the input matrix originally provided  
	                            # to cacheSolve
	  
	  invM <- solve(inputMat) #returns the inverse of the matrix stored in inputMat
	  
	  x$setInverse(invM) # Associate the inverse calculated from the last step with the original matrix
	                     # provided to the cacheSolve function
	  
	  invM
	 }
