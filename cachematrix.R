## Here lie a pair of functions. The first, makeCacheMatrix, is more or less
## a mimic of the example code provided. It accepts a matrix argument
## and contains a list of functions with which another object may
## interact with its contents. 
## In the name of science, and the assignment, its "greatest" feature
## is the ability to retain some data in this namespace
## Specifically, it keeps 3 datums: the "oldMatrix", "currentMatrix"
## and "cachedInverse" of the "oldMatrix". 
## The when oldMatrix and currentMatrix are the same, there have been
## no changes to the matrix for which the cachedInverse was calculated
## otherwise, the cachedInverse should be recalculated

## There may be some merit in assigning the cacheddInverse when this object is created.
## but if it is truly processor intensive, I can see an argument to NOT perform
## the calculation unless it is needed.

## That said, I have also added an initialization function to makeCacheMatrix that can be used
## to update the oldMatrix, currentMatrix, and cachedInverse
## In my code, this is run when the makeCachedMatrix object is created, as well as when
## the resetMatrix() function is run
## Actually, this makes the second function in this assignment unnecessary ;( 
## And makes some of the functions included here unnecessary. However because the function
## was assigned as homework, you will probably still want to look at both of the functions
## and see that they play well together.


## The list of functions allows interaction with these stored data
## 
## getMatrix() reports the most recent  matrix
## getOldMatrix() reports the values of the matrix for which the inverse was calculated
## resetMatrix() accepts a new working matrix 
## setInverse() sets the value to be cached for the inverse
## getInverse() reports the value of the cached Inverse


## Stores a matrix, and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #initialize some empty variables 
  currentMatrix<-NULL  #will hold active matrix, allowing resetting the matrix held in this object   
  cachedInverse<-NULL  #will hold cachedInverse matrix
  oldMatrix<-NULL      #will hold matrix for which cachedInverse was calculated
                       #allows recalculation of the cachedInverse if/when the matrix changes
  #this function is called to assign initial values from the matrix argument
  #it is called when the object is created, as well as when a new matrix is passed to the object
  
      initialize<-function(x){
            currentMatrix<<-x #store the new matrix
            cachedInverse<<-solve(x) #calculate the inverse
            #report that solve is being run, I did this to make sure solve wasn't being run when it wasn't supposed to be
            #print("the processor intensive part") 
            oldMatrix<<-x     #record that it is the one for which the inverse was calculated
      }

                                      
  initialize(x) #call the initialization function
 
 #here are the functions that allow another objects to interact with this one
        #This function lets the user change the matrix held in memory 
          resetMatrix<-function(y=matrix()){
                     currentMatrix<<-y             #save the new matrix data               
                     #if the new and old matrices are not the same, reset the cached values
                     if(!identical(currentMatrix,oldMatrix)){ 
                          initialize(y)    #reset the cachedInverse value,
                          #print("New Matrix!") #some comfortable feedback for debugging
                     } #end if(!identical)     
          }#end resetMatrix()
 
        #this function returns to current Matrix  
          getMatrix<-function() currentMatrix 
 
        #this function sets the matrix cached for the currentMatrix
          setInverse<-function(inverse){
                cachedInverse<<-inverse  #store the value passed in from the outside
                oldMatrix<<-currentMatrix #update the matrix for which the inverse was calculated
          } #end setInverse(inverse)
 
        #this function reports the cachedInverse for anyone interested in seeing it
          getInverse<-function() cachedInverse
 
        #this function reports the old matrix
          getOldMatrix<-function() oldMatrix
          

 
#return the list of available functions with which to interact with this object
  list(resetMatrix=resetMatrix,
      getMatrix=getMatrix,
      getInverse=getInverse,
      getOldMatrix=getOldMatrix,
      setInverse=setInverse)
}#end cachedMatrix


## This function operates on the makeCacheMatrix object to retrieve the cached inverse matrix.
## Although it has been made obsolete by some parts of the function I wrote above, it is still
## here, and served its pedagological purpose, I am almost sure.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getInverse()      #see if you can pull it from the object x
        oldMatrix<-x$getOldMatrix()  #check the value of the old matrix
        currentMatrix<-x$getMatrix() #check the value of the current matrix
        
        #if the inverse is not defined and the oldMatrix is the same as the currentMatrix 
        #(that is the matrix for which the inverse was calculated is the same as the one stored in the object)
        if(!is.null(inverse)&&identical(currentMatrix,oldMatrix)){ 
                #report that the value was not recalculated, and things look OK.
                message("cautious optimism")
                return(inverse) #and leave the function
          }
  
        #if those conditions are not met, a (new) inverse must be calculated
        inverse<-solve(currentMatrix) #calculate the inverse
        x$setInverse(inverse)   
        #x$updateOldMatrix(currentMatrix) #load the data
        #actually, this should be done in the other function

  inverse
}

