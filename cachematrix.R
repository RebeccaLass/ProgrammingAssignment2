## Put comments here that give an overall description of what your
## functions do
#the first function calculates the inverse of the matrix and cache it, the second function checks if the 
#inverse of the matrix was already cached in the first function or if it has to calculate the inverse itself.
## Write a short comment describing this function
#This function creates a matrix object that can cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {                     

        m <- NULL                       #setting m to Null
  set <- function(y) {                  #define set function
    x <<- y                             #assign new value of matrix in parent environment
    m <<- NULL                          #if there is a new matrix reset m to Null
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse         #assigns value of m in parent environment
  getinverse <- function() m                            #gets value of m
  list(set = set, get = get,
       setinverse = setinverse,                         #needed to refer to it with $ operator
       getinverse = getinverse)
}


## This function gives us the inverse of the Matrix we put in. First its checking if the inverse has already
## been calculated and if not, its calculating the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()           
  if(!is.null(m)) {                     #checking if it's not Null (inverse is already known)        
    message("getting cached data")      #printing out the message
    return(m)                           #return the inverse of the matrix
          
  }
  data <- x$get()                       #otherwise you get the original matrix
  m <- solve(data, ...)                 #invert the matrix
  x$setinverse(m)                       #set the inverse of the matrix m
  return(m)                             #return the inverse of the matrix m
}
