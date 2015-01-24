makeCacheMatrix <- function(x = matrix()) { 
  m<-NULL               
  set<-function(y){
    x<<-y #the given matrix gets stored in x ie value of input matrix is set
    m<<-NULL
  }
  get<-function() x #get the value of input matrix
  setmatrix<-function(solve) m<<- solve # now set inverse of given matrix
  getmatrix<-function() m #get the inverse of given matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() #caclulate the inverse of given matrix
  if(!is.null(m)){ # check if the inverse has already been calculated or not
    message("getting cached data") #
    return(m) # if yes then inverse from the cache is retrieved
  }
  matrix<-x$get() # else, get the value of input matrix
  m <- solve(matrix, ...) #computation of inverse of the input matrix
  x$setmatrix(m) #now I am setting the value of inverse of the input matrix
  m
}
#example
#a<-makeCacheMatrix()
#a$set(matrix(1:4,2,2))
#cacheSolve(a)      
