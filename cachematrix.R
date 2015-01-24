makeCacheMatrix <- function(x = matrix()) { 
  m<-NULL               
  set<-function(y){
    x<<-y #the given matrix gets stored in x
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve # now m = inverse of given matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){ # check if the matrix is null or not
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
#example
#a<-makeCacheMatrix()---initialize
#a$set(matrix(1:4,2,2))---input matrix
#cacheSolve(a)      
