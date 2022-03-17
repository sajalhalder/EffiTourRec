#========================================================================================================

# Description: These codes form part of our EffiTour algorithm for Efficient itinerary recommendation via personalized POI selection and pruning
#
# If we use these codes, have to cite the following paper:
# 1.) Sajal Halder, Kwan Hui Lim, Jeffrey Chan, Xiuzhen Zhang,  Efficient itinerary recommendation via personalized poi selection and pruning. Knowledge and Information Systems, pp 1–31, 2022
#========================================================================================================

# Add necessary library function
library(plyr)  
source("ModelFiles.R")   # Added necessary functions in ModelFiles 


# Define necessary variable   


maxLoop =100
SelectApproach = 1
traverPOINumber = 0


# This function returns a day is weekday or weekend. If weekday returns 1, otherwise returns 0. 
checkWeek <-function(x) { 
  weekday = c("Monday","TuesDay","Wednesday","Thursday","Friday")
  if(!x %in% weekday)
    return (0)
  else return (1)
  
}

start_time <- Sys.time()   # program strat time 

dfNodes = read.csv("Data/costProfCat-disHolly-all.csv", sep=";", header=TRUE) # Read costProfCat-disHolly-all.csv file in dfNodes as disHolly dataset. If you would like to use other dataset change the read file name based on  dataset 
#dfNodes$walkTime = dfNodes$walkTime
#dfNodes$rideDuration = dfNodes$rideDuration

dfVisits = read.csv("Data/userVisits-disHolly-allPOI.csv", sep=";", header=TRUE)  # Read user visit all POI data file in dfVisits
#dfVisits$rideDuration = dfVisits$rideDuration

############## Find Dynamic Popularity #######################################

dfVisits1 = dfVisits
Time = as.POSIXlt(dfVisits1[,"takenUnix"],origin = "1970-01-01")

dfVisits1$month = Time$mon+1
#dfVisits1$day = Time$mday
dfVisits1$hour = Time$hour       # Find starting hour  

dfVisits1$weekday = checkWeek(weekdays(Time, abbreviate = FALSE))
popMonth= count(dfVisits1, vars = c("poiID","month")) #Create userCount based on nsid and its frequency where number of attributes is 2 and number of records is 3342 
pophour = count(dfVisits1, vars = c("poiID","hour"))
popweek = count(dfVisits1, vars = c("poiID","weekday"))
names(popularity) = c("poiID","popularity") # Rename column header now its hear title is nsid and userFreq

######################################################################


dfQueueTimes = read.csv("Data/queueTimes-disHolly.csv", sep=";", header=TRUE)  # Read queue time data file in dfQueueTimes 
dfQueueTimes$avgQueueTime = dfQueueTimes$avgQueueTime

##   Get User Interest Function 
#======================================================
getUserInterest <-function(visitSeqId, dfVisits)
{
  userid = dfVisits[dfVisits$seqID==visitSeqId,]$nsid[1]   # Find one user node id whose sequence id is equal to given visitid
  
  dfOtherVisits = dfVisits[dfVisits$seqID!=visitSeqId & dfVisits$nsid==userid,]    # Find others visit sequence 
  
  userInterest = count(dfOtherVisits, vars="poiTheme")  # Find the neighbouring visiting theme point and their count value 
  
  names(userInterest) = c("category","catIntLevel")   # Theme category and their count create a matrics whose header is category and catIntLevel
  
  return (userInterest)   # Return user interest matrix 
  
}

inc <- function(x) {eval.parent(substitute(x <- x + 1))}
#======================================================

# Some Pre-Processing for dfVisits to extract unique visit sequence 

dfVisitsAll = dfVisits    # copy visits records in dfVisitsAll 
dfVisits = dfVisits[order(dfVisits$seqID,dfVisits$takenUnix,decreasing=FALSE),]   # Order the photo based on seqID and photo taken time Unit 
dfVisitsUserCount = dfVisits[!duplicated(dfVisits[,c('nsid','seqID')]), ] # Remove dupicate records that results it finds 8128 decords from 73994 records 
userCount = count(dfVisitsUserCount, vars="nsid") #Create userCount based on nsid and its frequency where number of attributes is 2 and number of records is 3342 
names(userCount) = c("nsid","userFreq") # Rename column header now its hear title is nsid and userFreq
dfVisits = merge(dfVisits, userCount, by="nsid")   # update dfVisits whose nsid is equal to userCound nsid . Now number of columns is 9 (add one column called userFreq )
dfVisitsSeqCount = dfVisits[!duplicated(dfVisits[,c('poiID','seqID')]),] # Removing duplicate records based on poiID and seqID. Now number of records 19606 from 73994
seqCount = count(dfVisitsSeqCount,vars="seqID")  # Find number of sequence and its frequencey 
names(seqCount) = c("seqID","seqFreq")   # Rename column of sequence id and its frequency
dfVisits = merge(dfVisits,seqCount, by = "seqID")   # Add sequenc frequency cloumn in the main datasets and now column = 10 and records = 73994 
dfVisits = dfVisits[order(dfVisits$seqID, dfVisits$takenUnix, decreasing = FALSE), ]  # update dfVisits based on sequence id and photo taken time 
dfVisits = dfVisits[dfVisits$userFreq >=2,]    # Remove those records which number of user frequency is less than 2. Now number of records is 58046
dfVisits = dfVisits[dfVisits$seqFreq >=3,]   # Remove number of records which sequence frequency is less than 3. 

#write.table(dfVisits, file = "Visits_data.csv", sep=";", row.names=FALSE, col.names=TRUE)  # Write the results in the file 

# For baseline algorithm

dfNodesBL = dfNodes  # Copy dfnodes in which number of attributes is 7 and number of records is 702
dfNodesBL$cost = dfNodesBL$walkTime + dfNodesBL$rideDuration   # Add one attribute name cost that's value is the sum of walktime and ride Duration. 
dfNodesBL$profit = dfNodesBL$popularity   # add another attribute name as profit in which value is equal to popularity value 
dfVisitsAllBL = dfVisitsAll   # Copy one original dfnodes dataset into dfVisitsAllBL 
dfVisitsAllBL$userID = dfVisitsAllBL$nsid   # Add one attribute name userID into dfVisitsAllBL
dfVisitsAllBL$dateTaken = dfVisitsAllBL$takenUnix  # add more attribute named dateTaken. Now number of attributes is 10 

results = data.frame()  # Create data frame name as results 

#Randomly test the various tourRecAlgos for each four sequence 

randomSeqID = sample(unique(dfVisits$seqID),length(unique(dfVisits$seqID)))   # Create random sequence list among the visits sequence. Here number of values 1739


for(tempSeqID in randomSeqID)
{
  exploreParam = 0.707168   # Constant Value   
  
  #********************* For Fixed Start and Destination Node **************
  
  dfTempVisits = dfVisits[dfVisits$seqID == tempSeqID,]   # Find the visit history of each sequence ID
  startNode = dfTempVisits[1,"poiID"]    # Find the start POI ID of each route 
  endNode = dfTempVisits[nrow(dfTempVisits),"poiID"]     # Find the end POI ID of each route 
  if(startNode == endNode)   {  next }  # If start poi ID is equal end poi ID then only one point is exist, there is no paths. And move to the next section
  
  #******************************************************************
  
  # set starting time (hour) of tour based on the actual starting hour
  
  startTime = as.POSIXlt(dfTempVisits[1,"takenUnix"],origin = "1970-01-01") # takenUnix time convert Date Time format  
  startHour = startTime$hour       # Find starting hour           
  
  #Calculate User interest 
  
  userInterest = getUserInterest(tempSeqID,dfVisitsAll)   # Call a function to find user user interst based on sequence ID and visits database
  
  #**************** For Fixed Budget **************************
  
  
  #***************** Maximum Travel time is considered as budget ******************** 
  budget = dfTempVisits[nrow(dfTempVisits),"takenUnix"]-dfTempVisits[1,"takenUnix"]     # Find the budget from the ending node time - starting node visiting time 
  print(paste0("startNode:",startNode, " ; endNode:", endNode, " ; budget:", budget, " ; startHour:", startHour))   # Print startnode, endnode, budget and start hour
  
  # Recommended tour before considering queuing time 
  
  solnMCTS = EffiTourRec(startNode, endNode, budget, dfNodes, dfQueueTimes, exploreParam, maxLoop, startHour,userInterest, traverPOINumber)
  
  
  if(nrow(solnMCTS)>0)  solnMCTS$seqID = tempSeqID    # set temp sequence id as solutions id
  
  #Recommended tour, after factoring in queuing timestamp
  #dfRecomTours = solnMCTS
  solnMCTS = addQueueTime(solnMCTS,  dfQueueTimes,startHour, budget)   # Call addQueuetime function 
  
  #Calculate the various results 
  stats = calcStatsQueue(solnMCTS, dfTempVisits, dfNodes, userInterest) 
  
  tempResults = data.frame(algo="MCTS", starNode=startNode, endNode=endNode, budget=budget, stats)  # Define temporary results data frame using algo, startNode, endNode, Budget and stats 
  
  results = rbind(results,tempResults)   #Update the results
  
  write.table(results, file = "disHolly-EffiTourRec.csv", sep=";", row.names=FALSE, col.names=TRUE)  # Write the results in the file 
}
end_time <- Sys.time()
Time = end_time - start_time
print(paste0("Start_Time = ", start_time))
print(paste0("End_Time = ", end_time))
print(paste0("Time = ", Time))  # Find the execution time 
write.table(time, file = "Execution_time.txt")






