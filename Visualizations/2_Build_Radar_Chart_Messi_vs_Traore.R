
require(ggplot2)

#Create a data frame from Messi and AdamaTraore's FIFA 2018 attribute data 

LionelMessi <- c(Pace = 96, Shooting = 97, Passing = 98, Dribbling = 99,Defending = 45,Physical = 81)
AdamaTraore <- c(Pace = 93, Shooting = 60, Passing = 59, Dribbling = 80,Defending = 24,Physical = 70)

# Unir por filas los dos jugadores
data <- rbind(LionelMessi, AdamaTraore)

Attributes = colnames(data)
AttNo = length(Attributes)

data <- cbind(data, data[,1])


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circle1 <- circleFun(c(0,0),200,npoints = 100)
circle2 <- circleFun(c(0,0),150,npoints = 100)
circle3 <- circleFun(c(0,0),100,npoints = 100)
circle4 <- circleFun(c(0,0),50,npoints = 100)

angle_spilt <- (2*pi) / (AttNo)
angle_spilt_seq <- seq(0,(2*pi),angle_spilt)

# empty dataframes to catch results 
LineData <- data.frame(x = numeric, y = numeric, stringsAsFactors = F)
TitlePositioning <- data.frame(title = character, x = numeric, y = numeric, stringsAsFactors = F)

## create plot background construction data  
for (i in 1:NCOL(data)) {
  angle_multiplier <- if(i < NCOL(data)){i}else{1}
  radians_for_segment <- angle_spilt_seq[i]
  
  x <- 100 * cos(radians_for_segment)
  y <- 100 * sin(radians_for_segment)
  temp <- data.frame(x = x, y = y, stringsAsFactors = F)
  LineData <- rbind(temp, LineData)
  
  x <- 112 * cos(radians_for_segment)
  y <- 112 * sin(radians_for_segment)
  title <- colnames(data)[i]
  temp <- data.frame(title = title, x = x, y = y, stringsAsFactors = F)
  TitlePositioning <- rbind(temp, TitlePositioning)
}

## create the value labellings data 
values <- c(25,50,75)
radian_for_values <- angle_spilt / 2
x <- values * cos(radian_for_values)
y <- values * sin(radian_for_values)
ValuePositioning <- data.frame(values = values, x = x, y = y, stringsAsFactors = F)

## Add the origin values for the lines 
LineData$x2 <- 0
LineData$y2 <- 0

# empty dataframe to catch result 
polydata <- data.frame(player = character, value = numeric, radians = numeric, x = numeric, y = numeric, stringsAsFactors = F)

## create polygon data for the players 
for (i in 1:NCOL(data)) {
  
  for (p in 1:NROW(data)) {
    
    player2calc <- data[p,]
    angle_multiplier <- if(i < NCOL(data)){i}else{1}
    radians_for_segment <- angle_spilt_seq[i]
    x <- player2calc[i] * cos(radians_for_segment)
    y <- player2calc[i] * sin(radians_for_segment)
    player <- rownames(data)[p]
    temp <- data.frame(player = player, value = player2calc[i], radians = radians_for_segment, x = x, y = y, stringsAsFactors = F)
    polydata <- rbind(temp, polydata)
  }
}

## split the data up into player 1 and 2
playersDB <- unique(polydata$player)
player1 <- polydata[which(polydata$player == playersDB[1]),]
player2 <- polydata[which(polydata$player == playersDB[2]),]

## create the title string for player 1
Player1_title <- gsub('([[:upper:]])', ' \\1', playersDB[1])
Player1_title <- trimws(Player1_title)

## Create Title Strings for Player 2
Player2_title <- gsub('([[:upper:]])', ' \\1', playersDB[2])
Player2_title <- trimws(Player2_title)



######################################
## PLOT RADAR CHART 1 MESSI VS TRAORE
#####################################

## Add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
  
  ## Add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + 
  geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + 
  geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + 
  geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") +
  
  ## Change the theme to void 
  theme_void() +
  
  ## Add the segment lines and attribute/value titles 
  geom_segment(data=LineData, aes(x = LineData$x, y = LineData$y, xend = LineData$x2, yend = LineData$y2),colour = "#d9d9d9", linetype = "dashed") + 
  annotate("text", x = TitlePositioning$x , y = TitlePositioning$y, label = TitlePositioning$title, size= 4, colour = "#4682B4",fontface = "bold") +  
  annotate("text", x = ValuePositioning$x , y = ValuePositioning$y, label = ValuePositioning$values, size= 3, colour = "#969696") +
 
   ## Add player 1 data 
  geom_polygon(data = player1, aes(x=x,y=y),fill = "#A30845", colour = "#A30845", alpha = 0.3) + 
  geom_point(data = player1, aes(x = x, y = y),size=0.3, colour= "#A30845") + 
  
  ## Add Chart Title
  annotate("text", x = -110 , y = 130, label = Player1_title, size= 5, colour = "#A30845", family = "Helvetica", fontface = "bold", hjust = 0) + 
  annotate("text", x = 110 , y = 130, label = "FIFA 18 Data", size= 4, colour = "#E84855", family = "Helvetica", fontface = "bold", hjust = 1) +
  
  ## Add the player 2 polygon and data points
  geom_polygon(data = player2, aes(x=x,y=y),fill = "#00B20B", colour = "#00B20B", alpha = 0.3) +
  geom_point(data = player2, aes(x = x, y = y),size=0.3, colour= "#00B20B") +
 
   ## Add the titles for player 2
  annotate("text", x = -110 , y = 116, label = Player2_title, size= 5, colour = "#00B20B", family = "Helvetica", fontface = "bold", hjust = 0) + 
  annotate("text", x = -110 , y = 123 , label = "vs", size= 4, colour = "#4682B4", family = "Helvetica",fontface = "bold", hjust = 0)



######################################
## PLOT RADAR CHART 2  Mbappe vs Sadio Mane
# https://www.goal.com/en/lists/fifa-19-ratings-bale-mbappe-top-20-fastest-players-game/1d04wkiigop801iqzs4mok3lbh#1kcc0c9llbxkg1htot5bcb07jh
#####################################


KylianMbappe<- c(Pace = 96, Shooting = 81, Passing = 79, Dribbling = 89,Defending = 39,Physical = 72)
SadioMane <- c(Pace = 94, Shooting = 80, Passing = 76, Dribbling = 87,Defending = 42,Physical = 73)

# Unir por filas los dos jugadores
data <- rbind(KylianMbappe, SadioMane)

Attributes = colnames(data)
AttNo = length(Attributes)

data <- cbind(data, data[,1])






# empty dataframes to catch results 
LineData <- data.frame(x = numeric, y = numeric, stringsAsFactors = F)
TitlePositioning <- data.frame(title = character, x = numeric, y = numeric, stringsAsFactors = F)

## create plot background construction data  
for (i in 1:NCOL(data)) {
  angle_multiplier <- if(i < NCOL(data)){i}else{1}
  radians_for_segment <- angle_spilt_seq[i]
  
  x <- 100 * cos(radians_for_segment)
  y <- 100 * sin(radians_for_segment)
  temp <- data.frame(x = x, y = y, stringsAsFactors = F)
  LineData <- rbind(temp, LineData)
  
  x <- 112 * cos(radians_for_segment)
  y <- 112 * sin(radians_for_segment)
  title <- colnames(data)[i]
  temp <- data.frame(title = title, x = x, y = y, stringsAsFactors = F)
  TitlePositioning <- rbind(temp, TitlePositioning)
}

## create the value labellings data 
values <- c(25,50,75)
radian_for_values <- angle_spilt / 2
x <- values * cos(radian_for_values)
y <- values * sin(radian_for_values)
ValuePositioning <- data.frame(values = values, x = x, y = y, stringsAsFactors = F)

## Add the origin values for the lines 
LineData$x2 <- 0
LineData$y2 <- 0

# empty dataframe to catch result 
polydata <- data.frame(player = character, value = numeric, radians = numeric, x = numeric, y = numeric, stringsAsFactors = F)

## create polygon data for the players 
for (i in 1:NCOL(data)) {
  
  for (p in 1:NROW(data)) {
    
    player2calc <- data[p,]
    angle_multiplier <- if(i < NCOL(data)){i}else{1}
    radians_for_segment <- angle_spilt_seq[i]
    x <- player2calc[i] * cos(radians_for_segment)
    y <- player2calc[i] * sin(radians_for_segment)
    player <- rownames(data)[p]
    temp <- data.frame(player = player, value = player2calc[i], radians = radians_for_segment, x = x, y = y, stringsAsFactors = F)
    polydata <- rbind(temp, polydata)
  }
}

## split the data up into player 1 and 2
playersDB <- unique(polydata$player)
player1 <- polydata[which(polydata$player == playersDB[1]),]
player2 <- polydata[which(polydata$player == playersDB[2]),]

## create the title string for player 1
Player1_title <- gsub('([[:upper:]])', ' \\1', playersDB[1])
Player1_title <- trimws(Player1_title)

## Create Title Strings for Player 2
Player2_title <- gsub('([[:upper:]])', ' \\1', playersDB[2])
Player2_title <- trimws(Player2_title)












## Add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
  
  ## Add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + 
  geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + 
  geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + 
  geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") +
  
  ## Change the theme to void 
  theme_void() +
  
  ## Add the segment lines and attribute/value titles 
  geom_segment(data=LineData, aes(x = LineData$x, y = LineData$y, xend = LineData$x2, yend = LineData$y2),colour = "#d9d9d9", linetype = "dashed") + 
  annotate("text", x = TitlePositioning$x , y = TitlePositioning$y, label = TitlePositioning$title, size= 4, colour = "#4682B4",fontface = "bold") +  
  annotate("text", x = ValuePositioning$x , y = ValuePositioning$y, label = ValuePositioning$values, size= 3, colour = "#969696") +
  
  ## Add player 1 data 
  geom_polygon(data = player1, aes(x=x,y=y),fill = "#ED2939", colour = "#ED2939", alpha = 0.3) + 
  geom_point(data = player1, aes(x = x, y = y),size=0.3, colour= "#ED2939") + 
  
  ## Add Chart Title
  annotate("text", x = -110 , y = 130, label = Player1_title, size= 5, colour = "#ED2939", fontface = "bold", hjust = 0) + 
  annotate("text", x = 110 , y = 130, label = "FIFA 19 Data", size= 4, colour = "#E84855", fontface = "bold", hjust = 1) +
  
  ## Add the player 2 polygon and data points
  geom_polygon(data = player2, aes(x=x,y=y),fill = "#00B20B", colour = "#00B20B", alpha = 0.3) +
  geom_point(data = player2, aes(x = x, y = y),size=0.3, colour= "#00B20B") +
  
  ## Add the titles for player 2
  annotate("text", x = -110 , y = 116, label = Player2_title, size= 5, colour = "#00B20B", fontface = "bold", hjust = 0) + 
  annotate("text", x = -110 , y = 123 , label = "vs", size= 4, colour = "#4682B4",fontface = "bold", hjust = 0)



