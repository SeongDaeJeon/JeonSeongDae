# transpose fn definition

mytranspose <- function(x){
  
  
  
  
  # input  vector
  if(is.vector(x)){
    x <- as.matrix(x)
    y <- matrix(1,nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)){
      for(j in 1 : ncol(x)){
        y[j,i] <- x[i,j]
      }
    }
    return(as.vector(y))
  }
  
  
  else if(is.data.frame(x)){
    x <- as.matrix(x)
    y <- matrix(1,nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)){
      for(j in 1 : ncol(x)){
        y[j,i] <- x[i,j]
      }
    }
    return(as.data.frame(y))
  }
  
  
  # input null
  else if(is.null(x)){return("null")}
  
  # input logical
  else if(is.logical(x)){return("logical")}
  
  
  # input matrix
  else if(is.matrix(x)){
    y <- matrix(1,nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)){
      for(j in 1 : ncol(x)){
        y[j,i] <- x[i,j]
      }
    }
    return(y)
  }  
}
