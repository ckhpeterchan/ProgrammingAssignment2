## The functions are developed to cahe the inverse of a matrix so that 
## its value can be used readily without computing it repeatedly

## The first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  #function to set the value of the matrix
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  #function to get the value of the matrix
  get <-function() x
  
  #function to set the value of the inverse
  setinv <-function(solve) inv <<- solve
  
  #function to get the value of the inverse
  getinv <-function() inv
  
  #compile the list of functions
  list(set = set, get = get, setinv= setinv, getinv = getinv)
  
}

##
## The second function computes the inverse of the special matrix returned by the 
## above function makeCacheMatrix. If the inverse has already been calculated
## this function can retrieve the inverse from the cache directly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <-x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}

##########################################
#Testing
x<-matrix(c(1,3,5,6,7,8,12,45,2), nrow=3, ncol=3)
x
solve(x)

#call the 1st function to create the relevant list
matr<-makeCacheMatrix(x)
#call the 2nd function to solve the matrix using the list created
cacheSolve(matr)



