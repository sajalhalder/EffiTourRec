source("byRef.R")

#Increment function, increament a variable value by 1. 
#=======================================================
inc <- function(x) {eval.parent(substitute(x <- x + 1))}

#***********************************************************************************************************#
# Tour Recommendation by Adaptive Monte Carlo Tree Search (with Queue Time Consideration) based 
# Efficient Tour Recommdation (EffiTourRec) System  
#-------------------------------------------------------------------------------
# Efficient tour recommendation algorithm based on the Orienteering Problem and solved using an adaptive Monte Carlo 
# Tree Search (MCTS) with Upper Confidence Bound (UCB), i.e., also known as an Upper Confidence Bound 
# applied to Trees (UCT) that combines MCTS with UCB
#
# @param startNode the initial node (place) where the tour will start from
# @param endNode the final node (place) where the tour will end at
# @param budgetCost the maximum budget (tourTime in seconds) this tour can incur
# @param dfNodes the input table in the format "fromNode", "toNode", "walkTime(sec)", "distance (metres)" "profit", "rideDuration(sec)"
# @param dfQueueTimes the input table in the format "poiID","hour","avgQueueTime"
# @param exploreParam a value of [0,1] to decide on no (0) or all (1) exploration 
# @param maxRuns number of iterations that the MCTS will run
# @param startHour the hour [0,23] at which the tour will start
# @param userInterest personalized user interest table in the format "category" and "catIntLevel"
#
# @return an ordered subset of "dfNodes" which is the optimum solution, if there is one
#***********************************************************************************************************#



EffiTourRec <- function(startNode, endNode, budgetCost, dfNodes, dfQueueTimes, exploreParam, maxRuns, startHour, userInterest,traverPOINumber){
  
  # Used for without pruing or pruing techeque  if value is 0 no pruning or 1 means pruing 
  ThirdPruning = 1
  # some standard MCTS parameter
  startNode = as.character(startNode)
  endNode = as.character(endNode)
  dfNodes$to = as.character(dfNodes$to)
  dfNodes$from = as.character(dfNodes$from)
  smallNum = 0.000000001
  
  byRef(traverPOINumber)
  
  # get the list of node IDs
  nodeList = order(unique(dfNodes$from))
  
  # normalize profit to [0,1] so that the UCB1 can be applied
  dfNodes = merge(dfNodes, userInterest, all.x=TRUE, by="category")
  dfNodesOrig = dfNodes
  dfNodes$catIntLevel[is.na(dfNodes$catIntLevel)] = 0
  dfNodes$catIntLevel = dfNodes$catIntLevel*dfNodes$popularity
  dfNodes$popularity = dfNodes$popularity*dfNodes$rideDuration/60
  
  
  
  dfNodes$profit = dfNodes$popularity + dfNodes$catIntLevel
  
  # store our MCTS tree as dataframe, where consecutive rows represent moving from one POI to another
  emptyTree = data.frame(matrix(0, ncol=length(nodeList), nrow=length(nodeList)))
  names(emptyTree) = nodeList # column names correspond to POI IDs/names
  #endTime = array(0, dim = c(as.integer(length(nodeList))+1,as.integer(length(nodeList))+1))
  
  treeVisitCount = emptyTree
  treeRewards = emptyTree
  ItineraryReward = emptyTree
  dfTourResults = data.frame(tourList=c(), tourReward=c(), tourCost=c()) # list of tours/rewards/costs that MCTS has explored
  
  #traverPOINumber = 0 
  # tour always begin from [startNode], so row 1 of [treeVisitCount] is the second POI to visit
  for ( i in 1:maxRuns ) {
    availNodeList = nodeList
    currNode = startNode
    tempVisitedTree = emptyTree
    tempRewardsTree = emptyTree
    tempCostTree = emptyTree
    recomTourList = c() # current list of recommended POIs (in sequence, minus the [startNode])
    recomTourReward = 0
    recomTourCost = 0
    
    if ( startNode!=endNode ) {
      availNodeList = availNodeList[ !availNodeList %in% startNode ]
    }
    Skip = 0 
    RemainTime = budgetCost
    
    for ( j in 1:nrow(treeVisitCount) ) {
      
      traverPOINumber = inc(traverPOINumber)
      currHour = (floor(sum(tempCostTree)/3600) + startHour) %% 24
      
      dfRoutes = dfNodes[dfNodes$from==currNode, ] # valid choices/POIs 
      
      
      dfTempQueueTimes = dfQueueTimes[dfQueueTimes$poiID %in% dfRoutes$to & dfQueueTimes$hour==currHour,]
      dfRoutes = merge(dfRoutes, dfTempQueueTimes[,c("poiID","avgQueueTime")], by.x="to", by.y="poiID", all=TRUE)  # add the queue time to the dataframe
      names(dfRoutes)[names(dfRoutes)=="avgQueueTime"] = "queueTime"
      
      # akin to the Selection and Expansion stages of MCTS
      childTotalVisits = unlist(treeVisitCount[j,])+smallNum # total visit count of child nodes
      childTotalReward = unlist(treeRewards[j,]) # total accumulated reward of child nodes
      currNodeVisitCount = 0 # default for root node (i.e., [startNode])
      if ( currNode!=startNode ) { # i.e., not the first recommended POI
        #print(paste0("Curr node = ",currNodeVisitCount, " currNode =",currNode))
        #print(paste0(" Visit List = ", childTotalVisits))
        currNodeVisitCount = childTotalVisits[childTotalVisits=currNode]
        #print(paste0("Curr node 2 = ",currNodeVisitCount))
      }
      
      # need to check listCost here, list of different lengths
      dfDummy = dfRoutes[1,] # dummy row to main number of POIs, subsequently removed
      dfDummy[1,]$to = currNode
      dfTempRows = rbind(dfRoutes, dfDummy)
      dfTempRows = dfTempRows[order(dfTempRows$to),]
      
      
      listCost = dfTempRows$walkTime + dfTempRows$rideDuration + dfTempRows$queueTime
      dfTempRows$tempCost = dfTempRows$walkTime + dfTempRows$rideDuration + dfTempRows$queueTime
      
      heuristicComponent = (dfTempRows$popularity + dfTempRows$catIntLevel) / ((dfTempRows$queueTime+dfTempRows$walkTime)/60)#dfTempRows$tempCost
      heuristicComponent = heuristicComponent / (max(heuristicComponent)+smallNum) # normalize it to [0,1] for ucb3
      exploitComponent = (childTotalReward / childTotalVisits) / (dfTempRows$tempCost/60) # (dfTempRows$queueTime+dfTempRows$walkTime)/60) #dfTempRows$tempCost # i.e., average reward of this arm (child node/POI)
      #print(paste0("ExploitComponent = ", length(exploitComponent), "Df temp =", length(dfTempRows$tempCost)))
      # exploitComponent = childTotalReward # (dfTempRows$queueTime+dfTempRows$walkTime)/60) #dfTempRows$tempCost # i.e., average reward of this arm (child node/POI)
      exploitComponent = exploitComponent / (max(exploitComponent)+smallNum) # normalize it to [0,1] for ucb3
      heuristicComponent = heuristicComponent + exploitComponent
      #heuristicComponent = heuristicComponent / (max(heuristicComponent)+smallNum) # normalize it to [0,1] for ucb3
      heuristicComponent = heuristicComponent /2 # (max(heuristicComponent)+smallNum) # normalize it to [0,1] for ucb3
      
      exploreComponent = 2 * exploreParam * sqrt( (2 *log(currNodeVisitCount+1)) / childTotalVisits ) # i.e., how much to favour exploiting unexplored arms (child nodes/POIs) 
      #exploreComponent = exploreComponent / (max(exploreComponent)+smallNum) # normalize it to [0,1] for ucb3
      
      combinedComponent = heuristicComponent + exploreComponent
      
      #combinedComponent = heuristicComponent + exploitComponent + exploreComponent
      combinedComponent = combinedComponent[dfRoutes$to]
      dfRoutes$profit = combinedComponent # apply UCB1
      dfRoutes$profit = dfRoutes$profit + runif(nrow(dfRoutes),0,smallNum) # small random no to tie break
      dfRoutes = dfRoutes[dfRoutes$to %in% availNodeList, ] # ignore already-visited nodes/POIs
      RemainTime = budgetCost-sum(tempCostTree)
      dfRoutes = dfRoutes[(dfRoutes$walkTime + dfRoutes$rideDuration + dfRoutes$queueTime) <= RemainTime, ] # ignore nodes/POIs that will exceed remaining budgetCost
      
      
      if ( nrow(dfRoutes)==0 ) { break }    
      
      maxIndice = which.max(dfRoutes$profit)
      selectedNode = as.character(dfRoutes[maxIndice,]$to) # select the best next POI/node
      
      nodeReward = (dfNodes[dfNodes$from==currNode & dfNodes$to==selectedNode, ]$profit) / (dfQueueTimes[dfQueueTimes$poiID==selectedNode & dfQueueTimes$hour==currHour,]$avgQueueTime/60)
      
      nodeCost = dfNodes[dfNodes$from==currNode & dfNodes$to==selectedNode, ]$walkTime + dfNodes[dfNodes$from==currNode & dfNodes$to==selectedNode, ]$rideDuration + dfQueueTimes[dfQueueTimes$poiID==selectedNode & dfQueueTimes$hour==currHour,]$avgQueueTime # walkingTime + rideDuration + queueTime
      
      # akin to the Simulation stage of MCTS (calculate outcome of choices)
      availNodeList = availNodeList[ !availNodeList %in% selectedNode ]
      recomTourList = c(recomTourList,selectedNode)
      recomTourReward = recomTourReward + nodeReward
      recomTourCost = recomTourCost + nodeCost
      tempVisitedTree[j,selectedNode] = 1
      tempRewardsTree[j,selectedNode] = nodeReward
      tempCostTree[j,selectedNode] = nodeCost
      
      if ( length(availNodeList)==0 | selectedNode==endNode) {break}
      
      if(ThirdPruning==1) 
      {
        if(ItineraryReward[j,as.character(selectedNode)]!=0 & ItineraryReward[j,as.character(selectedNode)]  >=  recomTourReward /(recomTourCost/60))
        {
          #print(paste0(" Valeu 1 = ", endTime[j,as.character(selectedNode)], " Value 2 = ", as.integer(nodeReward)))
          Skip = 1
          break 
        }
        
      }
      currNode = selectedNode
      
    }
    
    if(Skip == 1) {next}
    # akin to the Backpropagation stage of MCTS (rewards/outcomes are backpropagated)
    treeVisitCount = treeVisitCount + tempVisitedTree
    if ( length(recomTourList)==0 ) { 
      next
    }
    if ( recomTourList[length(recomTourList)]==endNode ) { # update reward only if [endNode] is reached
      
      #Code Added By Sajal
      #*******************************************************#
      #print (paste0(recomTourList))
      if(ThirdPruning==1) 
      {
        IR  = 0
        IC = 0 
        for(k in 1:length(recomTourList))
        {
          IR = IR + tempRewardsTree[k,as.character(recomTourList[k])] 
          IC = IC + tempCostTree[k,as.character(recomTourList[k])]/60
          ItineraryReward[k,as.character(recomTourList[k])]= IR /IC
          # print(paste0(" K = ",k , " item =", recomTourList[k], "endTime =",endTime[k,as.character(recomTourList[k])]))      
        }
      }
      #print (paste0(endTime))
      #*******************************************************#
      tempRewardsTree[tempRewardsTree>0] = sum(tempRewardsTree)
      treeRewards = treeRewards + tempRewardsTree
      dfTourResults = rbind(dfTourResults, data.frame(tourList=paste(recomTourList,collapse=","), tourReward=recomTourReward, tourCost=recomTourCost))
      
    }
    
  }
  
  #print(paste0(" Trever = ",traverPOINumber))
  if (nrow(dfTourResults)==0)
  {
    return (dfNodesOrig[0,]) # return an empty dataframe since no solution exists
  }
  dfTourResults = dfTourResults[with(dfTourResults, order(-tourReward)),]
  
  #print(paste0("Best Reward =",dfTourResults[1,]$tourReward))
  #print(paste0("Best Toure   =",dfTourResults[1,]$tourList))
  #print(paste0("Length of Solution =",nrow(dfTourResults)))
  
  bestTour = as.character(dfTourResults[1,]$tourList)
  bestTour = strsplit(bestTour, ",")[[1]]
  
  # convert the solution to readable form
  dfSoln = dfNodesOrig[0,]
  fromNode = startNode
  #print(paste0(" Start Node = ",fromNode))
  for ( i in 1:length(bestTour) ) {
    toNode = bestTour[i]
    #print(paste0(toNode))
    dfSoln = rbind(dfSoln, dfNodesOrig[dfNodesOrig$from==fromNode & dfNodesOrig$to==toNode,])
    fromNode = toNode
  }
  dfSoln$traverPOINumber = traverPOINumber
  #print(paste0("Total walkTime: ", sum(dfSoln$walkTime)))    
  #print(paste0("Total rideDuration: ", sum(dfSoln$rideDuration)))    
  #print(paste0("Total queueTime: ", sum(dfSoln$queueTime)))    
  #print(paste0("walkTime+rideDuration+queueTime: ", sum(dfSoln$walkTime) + sum(dfSoln$rideDuration) + sum(dfSoln$queueTime)))    
  #print(paste0("Total popularity: ", sum(dfSoln$popularity)))  
  #print(paste0("Total catIntLevel: ", sum(dfSoln$catIntLevel))) 
  
  return (dfSoln)
}




# function that takes in the [tourRecAlgo.r] output results and calculate various statistics like precision, recall, f1-score
# [solnRecTour] should be an output from calling addQueueTime(), i.e., with the queueTime and visitHour columns
calcStatsQueue <- function(solnMCTS, tempDfVisits, dfNodes, userInterest){
  # empty tour, just return it back
  solnRecTour = solnMCTS
  if ( nrow(solnRecTour)==0 ) {
    stats = data.frame(totalPOI=0, totalCost=0, totalWalkTime=0, totalRideDuration=0, totalQueueTime=0, totalPopularity=0, totalInterest=0, precision=0, recall=0, f1score=0, totalPopInt=0, totalPOImissed=0, normQueueTime=0, traverPOINumber = 0)
    return(stats)
  }
  
  # normalize the popularity and interest to [0,1]
  #dfNodes$popularity = dfNodes$popularity / max(dfNodes$popularity)
  #userInterest$catIntLevel = userInterest$catIntLevel / max(userInterest$catIntLevel)
  
  # calculate the total popularity and interest for the entire tour
  totalPOI = nrow(solnRecTour) + 1
  totalCost = 0
  totalWalkTime = 0
  totalRideDuration = 0
  totalQueueTime = 0
  totalPopularity = 0
  totalInterest = 0
  totalPopInt = 0
  totalPOImissed = solnRecTour$poiMissed[1]
  totalnormQueueTime = 0 # solnRecTour$normQueueTime[1]
  traverPOINumber = unique(solnRecTour$traverPOINumber)
  
  for (i in seq(1:nrow(solnRecTour))) {
    tempFrom = solnRecTour[i,]$from
    tempTo = solnRecTour[i,]$to
    totalnormQueueTime = totalnormQueueTime + solnRecTour$normQueueTime[i]
    #print(paste0(" Norm Queue = ", solnRecTour[i,]$normQueueTime))
    tempCost = dfNodes[dfNodes$from==tempFrom & dfNodes$to==tempTo,]$walkTime + dfNodes[dfNodes$from==tempFrom & dfNodes$to==tempTo,]$rideDuration + solnRecTour[solnRecTour$from==tempFrom & solnRecTour$to==tempTo,]$queueTime
    temptotalWalkTime = dfNodes[dfNodes$from==tempFrom & dfNodes$to==tempTo,]$walkTime
    temptotalRideDuration = dfNodes[dfNodes$from==tempFrom & dfNodes$to==tempTo,]$rideDuration
    temptotalQueueTime = solnRecTour[solnRecTour$from==tempFrom & solnRecTour$to==tempTo,]$queueTime
    tempPopularity = dfNodes[dfNodes$from==tempFrom & dfNodes$to==tempTo,]$popularity
    tempCategory = toString(dfNodes[dfNodes$from==tempFrom & dfNodes$to==tempTo,]$category)
    
    totalCost = totalCost + tempCost
    totalWalkTime = totalWalkTime+temptotalWalkTime
    totalRideDuration = totalRideDuration+temptotalRideDuration
    totalQueueTime = totalQueueTime+temptotalQueueTime
    
    totalPopularity = totalPopularity + tempPopularity
    if ( tempCategory %in% userInterest$category )
      totalInterest = totalInterest + userInterest[userInterest$category==tempCategory,]$catIntLevel
  }
  totalPopInt = 0.5*totalPopularity + 0.5*totalInterest
  
  # calculate the recall, precision and f1-score    
  visitSeq = unique(tempDfVisits$poiID)
  poiResults = c(solnRecTour[1,]$from, solnRecTour$to) %in% visitSeq # retrieve all POIs in the recommended tour, include the startNode since all tours start from the startNode
  precision = length(poiResults[poiResults==TRUE]) / length(poiResults)
  recall = length(poiResults[poiResults==TRUE]) / length(visitSeq)
  f1score = ((2*precision*recall) / (precision+recall))
  
  totalnormQueueTime = totalnormQueueTime/totalPOI
  #print(paste0("Norm Time  = normQueueTime", totalnormQueueTime))
  stats = data.frame(totalPOI=totalPOI, totalCost=totalCost, totalWalkTime=totalWalkTime, totalRideDuration=totalRideDuration, totalQueueTime=totalQueueTime, totalPopularity=totalPopularity, totalInterest=totalInterest, precision=precision, recall=recall, f1score=f1score, totalPopInt=totalPopInt, totalPOImissed=totalPOImissed, normQueueTime=totalnormQueueTime,traverPOINumber = traverPOINumber)
  
  return(stats)
}

#***********************************************************************************************************#
# Add Queuing Times to Recommended Tours
#--------------------------------------------------------------
# Takes the recommended tours [dfRecomTours] from baseline algorithms and adds the queuing time to each POI 
# visit based on the time of visit. POIs that could not be visited due to the addition of queuing times are 
# then dropped and an updated [dfCompletedTours] (subset of [dfRecomTours]) is returned.
#
# @param dfRecomTours the set of recommended tours (for all users, differentiated by seqID) in the format "fromNode", "toNode", "walkTime(sec)", "distance (metres)" "profit", "rideDuration(sec)", "seqID" 
# @param budget the total budget (sec) allocated for this tour
# @param dfQueueTimes the input table of queuing times in the format "poiID","hour","avgQueueTime"
#
# @return dfCompletedTours a subset of [dfRecomTours] where POI visits that exceeded the budget are removed
#***********************************************************************************************************#
addQueueTime <- function(dfRecomTours, dfQueueTimes, startHour, budget){
  
  # empty tour, just return it back
  if ( nrow(dfRecomTours)==0 )
    return (dfRecomTours)
  
  dfRecomTours$queueTime = NA
  dfRecomTours$visitHour = NA
  dfCompletedTours = dfRecomTours[0,] # list of completed POI visits after considering queuing time
  
  prevSeqID = dfRecomTours[1,]$seqID
  tourTotalTime = 0
  currHour = 0
  tempCost = 0 # cost in terms of total tour time (sec)
  
  for ( i in 1:nrow(dfRecomTours) ) {    
    if ( prevSeqID != dfRecomTours[i,]$seqID ) { # if different recommended tour
      prevSeqID = dfRecomTours[i,]$seqID
      tourTotalTime = 0
      currHour = 0
      tempCost = 0
    }
    
    currHour = (floor(sum(tempCost)/3600) + startHour) %% 24
    
    tempCost = tempCost + (dfRecomTours[i,]$walkTime + dfRecomTours[i,]$rideDuration + dfQueueTimes[dfQueueTimes$poiID==dfRecomTours[i,]$to & dfQueueTimes$hour==currHour,]$avgQueueTime)
    
    dfRecomTours$queueTime = dfQueueTimes[dfQueueTimes$poiID==dfRecomTours[i,]$to & dfQueueTimes$hour==currHour,]$avgQueueTime
    dfRecomTours$normQueueTime = dfQueueTimes[dfQueueTimes$poiID==dfRecomTours[i,]$to & dfQueueTimes$hour==currHour,]$avgQueueTime / max(dfQueueTimes[dfQueueTimes$poiID==dfRecomTours[i,]$to,]$avgQueueTime)
    dfRecomTours$visitHour = currHour
    
    # only add this POI visit if it is within budget after considering queuing time
    if ( tempCost < budget ) {
      dfCompletedTours = rbind(dfCompletedTours, dfRecomTours[i,])
    }
  }
  
  
  if ( nrow(dfCompletedTours)>0 )
    dfCompletedTours$poiMissed = nrow(dfRecomTours) - nrow(dfCompletedTours) # no of POIs missed, due to queue time
  
  return (dfCompletedTours)
}
