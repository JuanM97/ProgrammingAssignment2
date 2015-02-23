## Put comments here that give an overall description of what your
## functions do

# We will follow the programming style of the
# example provided in the description of the
# assignment...
makeCacheMatrix <- function(x = matrix()) {
  #ser the cached inverse to null
  inv <- null
  
  #set to matrix x the values of a new matrix y
  set<-function(y){
    x <<- y
    inv <<- null
  }
  
  #get the values of the matrix
  get<-function() x
  
  # Use function solve() to get the inverse of the
  # matrix and cache it
  setInverse<-function(inv2) inv<<-inv2
  
  #get value of the inverse of x
  getInverse()<-function() inv
  
  #Ret ...
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  
  #Check if already cached
  if(!is.null(inv)){
    message('getting cached inverse')
    #Return if already cached
    return(inv)
  }else{
  #Calc inverse and set it to inv(x)
    mat2<-x$get()
    inv<-solve(mat2,...)
    x$setInverse(inv)
    inv
  }
}


