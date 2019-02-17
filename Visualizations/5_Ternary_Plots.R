require(ggtern)


## Create an empty dataframe that we use to 'catch' the results from our while loop. 
Results <- data.frame(Club = character(), Player = character(), Pass = numeric(), Dribble = numeric(), Shoot = numeric(), stringsAsFactors = F)

## We define a few varibles here which will allow us to monitor our while loop as well as create club and player IDs
noPlayers <- 500 
noClubs <- 25 
PlayerCounter <- 1
ClubCounter <- 1
PlayersInTeamCounter <- 1

## Run a while loop for the number of players in our league
while(PlayerCounter <= noPlayers){
  
  Player <- paste0("Player",PlayerCounter) # create a playerID
  Club <- paste0("Club",ClubCounter) # create a club ID
  x <- runif(1,0,100) # create a random number
  y <- runif(1,0,(100-x)) # create another random number witbin the remaining range (100 - x)
  z <- 100 - (x+y) # calculate last number which is simply 100 - the first two numbers  
  
  # create a dataframe for this one player
  temp <- data.frame(Club = Club, Player = Player, Pass = x, Dribble = y, Shoot = z, stringsAsFactors = F)
  
  # bind the player results to the overall league results 
  Results <- rbind(Results, temp)
  
  # Add 1 to the player counter as we have processed another player
  PlayerCounter <- PlayerCounter + 1
  
  # Add 1 to the players in the team counter
  PlayersInTeamCounter <- PlayersInTeamCounter + 1
  
  # Work out it the team contains 25 players
  UpTeam <- if((PlayersInTeamCounter / noClubs)%%1==0){TRUE}else{FALSE}
  
  # Up date the players in the team counter 
  PlayersInTeamCounter <- if((PlayersInTeamCounter / noClubs)%%1==0){1}else{PlayersInTeamCounter}
  
  # If the players were more than 25 then update the club counter 
  ClubCounter <- if(UpTeam){ClubCounter + 1}else{ClubCounter}
}

## Print the top 5 rows to see if everything worked ok
head(Results)

############
# TERN
############



#################
## PLOT 1
#################

#Create the plot and store
plot <- ggtern(data = Results, aes(x = Pass, y = Dribble, z = Shoot)) + 
  geom_point(size = 2, 
             shape = 21, 
             color = "black",
             fill="black")

#Render
plot

#################
## PLOT 2
#################

PlayerSelect <- "Player27"
playerHightlight <- Results[which(Results$Player == PlayerSelect),]

require(ggtern)
#Create the plot and store
plot <- ggtern(data = Results, aes(x = Pass, y = Dribble, z = Shoot)) + 
  geom_point(size = 2, 
             shape = 21, 
             color = "black",
             fill= "black") + 
  geom_point(data = playerHightlight, aes(x = Pass, y = Dribble, z = Shoot), 
             size = 2, 
             shape = 21, 
             color = "black",
             fill= "red")

#Render
plot




#################
## PLOT 3
#################


#Create the plot and store
plot <- ggtern(data = Results, aes(x = Pass, y = Dribble, z = Shoot)) + 
  geom_point(size = 2, 
             shape = 21, 
             color = "grey",
             fill= "grey") + 
  geom_point(data = playerHightlight, aes(x = Pass, y = Dribble, z = Shoot), 
             size = 2, 
             shape = 21, 
             color = "black",
             fill= "red")

## Warning: Ignoring unknown aesthetics: z

#Render
plot


##########################
## PLOT 4  - CLUB 5
##########################

ClubSelect <- "Club5"
ClubHightlight <- Results[which(Results$Club == ClubSelect),]

require(ggtern)
#Create the plot and store
plot_club <- ggtern(data = Results, aes(x = Pass, y = Dribble, z = Shoot)) + 
  geom_point(size = 2, 
             shape = 21, 
             color = "grey",
             fill= "grey") + 
  geom_point(data = ClubHightlight, aes(x = Pass, y = Dribble, z = Shoot), 
             size = 2, 
             shape = 21, 
             color = "black",
             fill= "red")

## Warning: Ignoring unknown aesthetics: z

#Render
plot_club




# K-Means Clustering

# K-means Clustering is a way of dividing datapoints into groups or 'clusters' depending on how 'similar' they.
# A quick google will give you much more details, but let's see it in action with out fake data.

# create a new dataframe of just the passing, shooting and dribbling data. 
ResultsKmean <- Results[3:5]

# use the kmeans() function to perform the analysis, we chose to devide the data into 6 clusters 
kmeans <- kmeans(ResultsKmean, 6)

# We want to bind the kMeans Clusters to the original dataframe 
Results$Cluster <- as.character(kmeans$cluster)

## print the top 5 rows to check we are on the right path 
head(Results)

##    Club  Player      Pass   Dribble      Shoot Cluster
## 1 Club1 Player1 83.989731 10.404360  5.6059098       1
## 2 Club1 Player2 72.140048 27.267614  0.5923382       6
## 3 Club1 Player3  8.271803 82.067260  9.6609370       4
## 4 Club1 Player4 96.656463  1.475396  1.8681409       1
## 5 Club1 Player5 47.617233 19.929625 32.4531419       2
## 6 Club1 Player6 34.791456 49.700572 15.5079716       3

# Great now we have all the data and the clusters of our players. 
# Let's plot the full dataset again but colour it by cluster.

#Create the plot and store
plot_final <- ggtern(data = Results, aes(x = Pass, y = Dribble, z = Shoot)) + 
  geom_point(aes(fill = Cluster), size = 2, 
             shape = 21, 
             color = "black")

#Render
plot_final




# We can see that players within Cluster 3 have a high tendancy to Pass with over 80% of their actions being passes.
# Maybe we are about to sell our best midfielder who dictates and maintains our possessions.
# If we were looking across the league for a replacement we could focus our efforts on players that also reside in Cluster 3.
# Granted we are using fake data but this article may spark a thought about how you could use ternary plots and/or k-means clustering with other data.
