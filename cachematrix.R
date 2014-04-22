#so this code here returns the inverse of a matrix.
#it first checks if there is a cache inverse already. if it finds,
# then returns the cache value

# makeCacheMatrix function makes a matrix that can cache its inverse matrxix.
# ittakes an argument x which is empty matrix at this moment.
# it has 4 functioncs working inside of it
makeCacheMatrix<-function(x=matrix())
{
  inverse<-NULL # i set the value of the of inverse to be null. this works as a placeholder for future values
  set<-function(y)
  {
    x<<-y # i assigned the value of argument to x object
    inverse<<-NULL #inverse value is again reset to null.
  }
  get<-function() x # returns the matrix x
  setinv<-function(inv) inverse<<-inv #i set the inverse to inv
  getinv<-function() inverse # returns the inverse 
  list(set= set,
       get= get,
       setinv= setinv,
       getinv= getinv)
}
# cacheSolve function computes the inverse of the 
# matrix returned by makeCacheMatrix above. If the inverse has
# been calculated  beforehand (and the matrix has not changed), then
# cacheSolve should return the inverse from the cache. 


cacheSolve<-function(x,...)
{
  inverse<-x$getinv()
  #if checks if the inverse has already been calculated 
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix<-x$get()
  inverse<-solve(matrix,...)
  x$setinv(inverse)
  inverse
}
