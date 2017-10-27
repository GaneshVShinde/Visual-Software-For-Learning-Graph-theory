#source("~/MTECH/1stSem/R/graph.R")
library(igraph)
library(Matrix)
setClass (Class = "Graph" ,
          representation ( mats ="matrix" ,totalNodes = 'numeric') )

#class methods need to be part of generic methods
BFS <- function(graph) 0
setGeneric("BFS")

plotGraph <- function(graph) 0
setGeneric("plotGraph")

DFS <- function(graph) 0
setGeneric("DFS")

#constructor 
Graph=function(n)
{
  vals= (round(matrix(runif(n*n,min=0,max=1),n,n)))
  vals=utility$removeIsolatedNodes(vals)
  dimnames(vals) =list(rownames(vals,do.NULL = FALSE,prefix = 'A'),colnames(vals,do.NULL = FALSE,prefix = 'A'))
  #making symetric
  vals[lower.tri(vals)] = (t(vals))[lower.tri(vals)]
  diag(vals) = 0
  return(new('Graph',mats=vals,totalNodes =length(vals[1,])))
}

#class methods
setMethod("BFS", signature(graph = "Graph"), 
                  function(graph)
                  {
                    #graph = object
                    visited=c()
                    goneVisit=c()
                    notVisited = row.names(graph@mats)
                    visitAllNodes=0
                    goneVisit[1] = notVisited[1]
                    x=0;
                    i=1
                    numEdges=sum(graph@mats)
                    while(length(visited) <length(graph@mats[1,]) & i<(numEdges+length(graph@mats[1,])+1))
                    {
                      xxxx=names(which(graph@mats[goneVisit[1],]==1))
                      visited=utility$checkUniqueAndAppendToVec(visited,goneVisit[1])
                      #visited[i] = goneVisit[1]
                      goneVisit = utility$checkUniqueAndAppendToVec(goneVisit,xxxx)
                      goneVisit = goneVisit[goneVisit != goneVisit[1]]
                      cat("\n****** Visited ******\n")
                      cat(visited)
                      i=i+1
                    }
                    if(i>=numEdges+length(graph@mats[1,]))
						warning("kaunu lafda he ka may be more than one graphs exist")
					return(visited)
                  }
)



setMethod("plotGraph", signature(graph = "Graph"),
          function(graph)
          {
            relationVecs=utility$getRelations(graph@mats);
            graphicsObj=graph(edges=relationVecs,directed=F) # this is graph graphis object
            plot(graphicsObj)
          }
)

setMethod("DFS", signature(graph = "Graph"),
          function(graph)
          {
            
            return(utility$goTillEndOfGraph(graph@mats))
          }
)

#need revision for utility functions

main = function(n)
{
	GraphObj=Graph(n)
	plotGraph(GraphObj)
	cat("\n\n!!!!!!!!!!!!! BFS !!!!!!!!!!!!!!!\n\n")
	BFS(GraphObj)
	
	cat("\n\n!!!!!!!!!!!!! DFS !!!!!!!!!!!!!!!\n\n")
	DFS(GraphObj)
	
	return(GraphObj)
}




utility=new.env()

utility$checkUniqueAndAppendToVec = function(appendInto,valsToAppend,isDFS = FALSE)
{
  isUniqueAdded =FALSE
  if(length(valsToAppend)>0)
  {
	  for(i in 1:length(valsToAppend))
	  {
		if(!(valsToAppend[i] %in% appendInto))
		{
			if(isDFS == TRUE)
			{
				#print(valsToAppend[i])
				appendInto = append(c(valsToAppend[i]),appendInto)
			}
			else
				appendInto = append(appendInto,valsToAppend[i])
			isUniqueAdded =TRUE
		}
	  }
  }
  if(isDFS == TRUE)
    return(list(appendInto,isUniqueAdded))
  return(appendInto)
}


utility$getRelations = function(mats)
{
  allNodeNames = row.names(mats)
  relationList = list()
  for( i in 1:length(allNodeNames))
  {
    mats[lower.tri(mats)]=0
    relationalNodes=names(which(mats[allNodeNames[i],]==1))
    testVec = c()
    index=1
    if(length(relationalNodes)>0)
    {
		for(j in 1:length(relationalNodes))
			{
			  testVec[index] =allNodeNames[i]
			  index = index+1
			  testVec[index] =relationalNodes[j]
			  index = index+1
			}
    }
    
    relationList[[i]] = testVec      
  }
  return(unlist(relationList))
 }
  
  
utility$removeIsolatedNodes = function(mats)
{
	nR = nrow(mats)
	nC = ncol(mats)
	if(nR == nC | nR ==1)
	{
		for(i in 1:nR)
		{
			if(sum(mats[i,])==0)
				{
					if(i==nC)
						mats[1,nC] =1
					else
						mats[i,i+1] =1
				}
		}
	}
	else
	{
		warning("kya babua ur mats len is just 1 or nrow != ncol")
	}
	return(mats)
}


utility$goTillEndOfGraph = function(mats,node="A1")
{
  visited=c()
  inPlay=node
  i=0
  while(i<=(length(mats[1,])-1))
  {
    newChilds = names(sample(which(mats[inPlay[1],]==1)))
    vacs=utility$checkUniqueAndAppendToVec(utility$checkUniqueAndAppendToVec(inPlay,visited),newChilds,TRUE)
    if(vacs[2] == FALSE )
    {
	  cat("**** visited****\n")
	  visited=append(visited,inPlay[1])
      inPlay = inPlay[2:(length(inPlay))]
      i=i+1
      cat(visited)
	  cat("\n")
    }
    else
    {
      inPlay = unlist(vacs[1])
    }
  }
  return(visited)
}


main(10)
