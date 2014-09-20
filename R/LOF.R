LOF <-
function(data,k) {
  
    # source: library(dprep)
  
    # A function that finds the local outlier factor (Breunig,2000) of
    # the matrix "data" with k neighbors
    # Adapted by Caroline Rodriguez and Edgar Acuna, may 2004

  knneigh.vect <-
    function(x,data,k)
    {
      #Function that returns the distance from a vector "x" to   
      #its k-nearest-neighbors in the matrix "data"
      
      temp=as.matrix(data)
      numrow=dim(data)[1]
      dimnames(temp)=NULL
      
      #subtract rowvector x from each row of data
      difference<- scale(temp, x, FALSE)
      
      #square and add all differences and then take the square root
      dtemp <- drop(difference^2 %*% rep(1, ncol(data)))
      dtemp=sqrt(dtemp)
      
      #order the distances
      order.dist <- order(dtemp)
      nndist=dtemp[order.dist]
      
      #find distance to k-nearest neighbor
      #uses k+1 since first distance in vector is a 0
      knndist=nndist[k+1]
      
      #find neighborhood
      #eliminate first row of zeros from neighborhood 
      neighborhood=drop(nndist[nndist<=knndist])
      neighborhood=neighborhood[-1]
      numneigh=length(neighborhood)
      
      #find indexes of each neighbor in the neighborhood
      index.neigh=order.dist[1:numneigh+1]
      
      # this will become the index of the distance to first neighbor
      num1=length(index.neigh)+3
      
      # this will become the index of the distance to last neighbor
      num2=length(index.neigh)+numneigh+2
      
      #form a vector
      neigh.dist=c(num1,num2,index.neigh,neighborhood)
      
      return(neigh.dist)
    }
  
  
    
    dist.to.knn <-
      function(dataset,neighbors)
      {
        
        #function returns an object in which each column contains
        #the indices of the first k neighbors followed by the
        #distances to each of these neighbors
        
        numrow=dim(dataset)[1]
        
        #applies a function to find distance to k nearest neighbors
        #within "dataset" for each row of the matrix "dataset"
        
        knndist=rep(0,0)
        
        
        for (i in 1:numrow)
        {
          #find obervations that make up the k-distance neighborhood for dataset[i,]
          neighdist=knneigh.vect(dataset[i,],dataset,neighbors)
          
          #adjust the length of neighdist or knndist as needed to form matrix of neighbors
          #and their distances
          if (i==2)
          {
            if (length(knndist)<length(neighdist)) 
            {
              z=length(neighdist)-length(knndist)
              zeros=rep(0,z)
              knndist=c(knndist,zeros)
            }
            else if (length(knndist)>length(neighdist))
            {
              z=length(knndist)-length(neighdist)
              zeros=rep(0,z)
              neighdist=c(neighdist,zeros)
            }
          }
          else 
          {
            if (i!=1)
            {
              if (dim(knndist)[1]<length(neighdist)) 
              {
                z=(length(neighdist)-dim(knndist)[1])
                zeros=rep(0,z*dim(knndist)[2])
                zeros=matrix(zeros,z,dim(knndist)[2])
                knndist=rbind(knndist,zeros)
              }
              else if (dim(knndist)[1]>length(neighdist)) 
              {
                z=(dim(knndist)[1]-length(neighdist))
                zeros=rep(0,z)
                neighdist=c(neighdist,zeros)
              }
            }
          }           
          knndist=cbind(knndist,neighdist)         
        }
        
        return(knndist)
      }
  
  
  reachability <-
    function(distdata,k)
    {
      #function that calculates the local reachability density
      #of Breuing(2000) for each observation in a matrix, using
      #a matrix (distdata) of k nearest neighbors computed by the function dist.to.knn2
      
      p=dim(distdata)[2]
      lrd=rep(0,p)
      
      for (i in 1:p)
      {
        j=seq(3,3+(distdata[2,i]-distdata[1,i]))
        # compare the k-distance from each observation to its kth neighbor
        # to the actual distance between each observation and its neighbors
        numneigh=distdata[2,i]-distdata[1,i]+1
        temp=rbind(diag(distdata[distdata[2,distdata[j,i]],distdata[j,i]]),distdata[j+numneigh,i])
        
        #calculate reachability
        reach=1/(sum(apply(temp,2,max))/numneigh)
        lrd[i]=reach
      }
      lrd
    }
  
    
    data=as.matrix(data)
    
    #find k nearest neighbors and their distance from each observation
    #in data
    distdata=dist.to.knn(data,k)
    p=dim(distdata)[2]
    
    #calculate the local reachability density for each observation in data
    lrddata=reachability(distdata,k)
    
    lof=rep(0,p)
    
    #computer the local outlier factor of each observation in data
    for ( i in 1:p)
    {
      nneigh=distdata[2,i]-distdata[1,i]+1
      j=seq(0,(nneigh-1))
      local.factor=sum(lrddata[distdata[3+j,i]]/lrddata[i])/nneigh
      lof[i]=local.factor
    }
    
    #return lof, a vector with the local outlier factor of each observation
    lof
}
