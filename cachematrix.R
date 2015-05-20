#makeCacheMatrix: This function creates a special "matrix" object that can cache its
#inverse.
#Declaring the makeCacheMatrix function and allowing a matrix to be passed in
#as an argument. We are assuming that the matrix being supplied is always
#inervtible in this scenario.

makeCacheMatrix <- function(x = matrix()) {
  #Declaring matrix inverse as null 
  inverse <- NULL
  #Set allows the inverse matrix to be set, the <<- allows the 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #Returns the value of the matrix that was passed in or cached.
  get <- function() {
    x
  }
  
  #Sets setSolved to the value of the passed in matrix.
  setSolved <- function(solvedInverse) {
    inverse <<- solvedInverse
  }
  
  #getSolved returns the returns the cached matrix (inverse) or NULL if it hasn't
  #been populated yet.
  getSolved <- function() {
    inverse
  }
  
  #Returns a list of all of the functions.
  list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}

#Start cacheSolve
#Computes the inverse of a matrix by first seeing if the cached matrix
#alerady exists. If it already exists the cache value will be returned, if
#the inverse has not yet been calculated the inverse will be solved.
cacheSolve <- function(x, ...) {
  
  #Calling the getSolved function and returning the value of the inverse. If it is not
  #null then the cached value is returned since it has already been calculated.
  inverse <- x$getSolved()
  if(!is.null(inverse)) {
    message("getting cached data")
    #return the cached matrix
    return(inverse)
  }
  #Grabs the value in the matrix that was passed in.
  data <- x$get()
  #Calculates the inverse of the matrix.
  inverse <- solve(data)
  #Caches the value of the matrix inverse.
  x$setSolved(inverse)
  #Returns the inverse to the user.
  inverse
}