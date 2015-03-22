##The below function takes a matrix as input, which is assumed to be inversible
## Then the below funciton returns a list with 4 funcitons
## 1. set: that sets the matrix to new values
## 2. get: returns the values of the matrix
## 3. setinvmatrix: this sets the inverse of the matrix
## 4. getinvmatrix: returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  
  set <- function(y){
    x <<- y
    invm <<- NULL
  }
  
  get <- function() x
  
  setinvmatrix <- function(new_inv) invm <<- new_inv
  getinvmatrix<-function() invm
  
  list(set = set, 
       get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## The below function first checks if the inverse of the matrix is already calculated
## if the inverse is calculated, then returns it
## if not, then calcualtes the inverse of the matrix and returns it

cacheSolve <- function(x, ...) {
  ## check if the matrix has inverse of it calculated
  invm <- x$getinvmatrix()
  
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  temp_mat <- x$get()
  invm <- solve(temp_mat)
  
  x$setinvmatrix(invm)
  
  invm
}
