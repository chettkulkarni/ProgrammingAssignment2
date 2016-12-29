## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

#initialising inverse object to be used later 
inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
#setting up getter of object
  get<-function(){
    x
  }
#setting up setter to set the inverse to Inverse object  
  setInverse<-function(Inv){
    inverse<<-Inv
  }
#setting up getter of inverse of a matrix
  getInverse<-function(){
    inverse
  }
#assign all the functions as a list to be accessed outside with $  
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

#get the inverse of x  
inverse<-x$getInverse()
  if(!is.null(inverse)){
    #if inverse is not null ,it means already there is already a matrix inverse cached

    message("getting INVERSE OF MATRIX WHICH WAS CACHED")
    return(inverse)#return the cached inverse of the matrix
  }
#calculate inverse
  data<-x$get()
  inverse<-solve(data, ...)
#set the inverse for future reference
  x$setInverse(inverse)
  inverse

}
