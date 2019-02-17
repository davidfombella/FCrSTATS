## load packages 
require(dplyr) ## package for data tidying
require(formattable) ## package to make tables look nice
require(ggplot2) ## package for plotting

# devtools::install_github("thomasp85/patchwork")
require(patchwork) ## package for organising our plots

require(plyr) ## for help with summaries
require(tidyverse)

###########################################
# Install StatBombR using devtools package
###########################################
# devtools::install_github("statsbomb/StatsBombR")
require(StatsBombR)


# Load Free events from statbombs API
events <- StatsBombR::StatsBombFreeEvents()

#save(events,file="SB_All_events.Rda")

df<-  events %>%
         filter(match_id==8658)


# save(df,file="france_vs_croatia_wc_final.Rda")


# Next we store a list of the participlated teams which we will use as variables to filter data later.

Teams <- unique(df$team.name)



# Then we create a dataset for Team 1 (which in this case is France) and filtering out everything 
# but passes and filter out goal-kicks.

df_pass_T1 <- df %>% 
  filter(type.name == "Pass", team.name == Teams[1], play_pattern.name != "From Goal Kick")%>%
  separate(location,c("location","x","y"))%>%
  mutate(x=as.numeric(as.character(x)),y=as.numeric(as.character(y)))%>%
  separate(pass.end_location,c("pass.end_location","x_end","y_end"))%>%
  mutate(x_end=as.numeric(as.character(x_end)),y_end=as.numeric(as.character(y_end)))



# I want to make a summary table of the top 3 passers for France which
# I will use as part of my player selection method.


PassTotals <- count(df_pass_T1, 'player.name')
PassTotals <- PassTotals %>% arrange(-freq) %>% head(3)
formattable(PassTotals)


##################################
# Plotting the Team Histogram
##################################

# Firstly, let's plot the histogram for the whole team, 
# get the styling right and then overlay Pogba's histogram at the end.
#  We will use ggplot2 and we will build the plot layer by layer

##############################
# 1st base plot, confirm the df we will be using and the metric we will plot (x) which is vertical origin of the pass.  
##############################

ggplot(data=df_pass_T1, aes(df_pass_T1$x))


##############################
# 2nd Add histogram
##############################

ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF")


##############################
# 3rd Add histogram + x y limits for axis
##############################

ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125))


##############################
# 4th Add theme void
##############################

ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  ggtitle(paste0(Teams[1],": Vertical Origin of Passes (x)")) +
  theme_void()


##############################
# 5th Add Pogba
##############################

## create the player dataframe Pogba
df_player_selectT1 <- df_pass_T1 %>% filter(player.name == PassTotals$player.name[1])

##
ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  ggtitle(paste0(Teams[1],": Vertical Origin of Passes (x)")) +
  theme_void() + 
  geom_histogram(data = df_player_selectT1, aes(df_player_selectT1$x), binwidth = 5, alpha = 1, fill = "#0074D9")




##############################
# 6th Add Attacking direction
##############################

## create the player dataframe 
df_player_selectT1 <- df_pass_T1 %>% filter(player.name == PassTotals$player.name[1])

ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  ggtitle(paste0(Teams[1],": Vertical Origin of Passes (x) and ",PassTotals$player.name[1])) +
  theme_void() + 
  geom_histogram(data = df_player_selectT1, aes(df_player_selectT1$x), binwidth = 5, alpha = 1, fill = "#0074D9") + 
  geom_segment(aes(x = -2.5, y = -2, xend = 25, yend = -2),colour = "#435366", arrow = arrow(length = unit(0.1, "cm"), type="closed")) + 
  annotate("text", x = -2.5, y = -1, label = "Attacking Direction", colour = "#435366", fontface=2, size = 3, hjust = 0)






##############################
# 7th Next we can add some information on the y-axis to provide frequency context.
##############################


## create the player dataframe 
df_player_selectT1 <- df_pass_T1 %>% filter(player.name == PassTotals$player.name[1])

## create the dataframe that we will use to plot the y-axis ticks and text
df_ticks <- data.frame(x = -4, y = seq(10,40,10), xend = -2.5, yend=seq(10,40,10), label_text = as.character(seq(10,40,10)), stringsAsFactors = F )
df_ticks$label_text <- paste0(df_ticks$label_text, " ")

##
ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  ggtitle(paste0(Teams[1],": Vertical Origin of Passes (x) and ",PassTotals$player.name[1])) +
  theme_void() + 
  geom_histogram(data = df_player_selectT1, aes(df_player_selectT1$x), binwidth = 5, alpha = 1, fill = "#0074D9") + 
  geom_segment(aes(x = -2.5, y = -2, xend = 25, yend = -2),colour = "#435366", arrow = arrow(length = unit(0.1, "cm"), type="closed")) + 
  annotate("text", x = -2.5, y = -1, label = "Attacking Direction", colour = "#435366", fontface=2, size = 3, hjust = 0) + 
  geom_segment(data = df_ticks, aes(x = x, y=y, xend = xend, yend =yend), colour = "#435366") + 
  geom_text(data = df_ticks, aes(x = x, y=y,label = label_text), hjust=1, vjust=0.5, size = 3)





##############################
# 8th We don't know which player the plot refers to! So let's add the name of the player and a pass count in [].
##############################


## create the player dataframe 
df_player_selectT1 <- df_pass_T1 %>% filter(player.name == PassTotals$player.name[1])

## create the dataframe that we will use to plot the y-axis ticks and text
df_ticks <- data.frame(x = -4, y = seq(10,40,10), xend = -2.5, yend=seq(10,40,10), label_text = as.character(seq(10,40,10)), stringsAsFactors = F )
df_ticks$label_text <- paste0(df_ticks$label_text, " ")

##
ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  ggtitle(paste0(Teams[1],": Vertical Origin of Passes (x)")) +
  theme_void() + 
  geom_histogram(data = df_player_selectT1, aes(df_player_selectT1$x), binwidth = 5, alpha = 1, fill = "#0074D9") + 
  geom_segment(aes(x = -2.5, y = -2, xend = 25, yend = -2),colour = "#435366", arrow = arrow(length = unit(0.1, "cm"), type="closed")) + 
  annotate("text", x = -2.5, y = -1, label = "Attacking Direction", colour = "#435366", fontface=2, size = 3, hjust = 0) + 
  geom_segment(data = df_ticks, aes(x = x, y=y, xend = xend, yend =yend), colour = "#435366") + 
  geom_text(data = df_ticks, aes(x = x, y=y,label = label_text), hjust=1, vjust=0.5, size = 3) +  
  annotate("text", x = 60, y = -4, label = paste0(PassTotals$player.name[1]," [",PassTotals$freq[1],"]"), colour = "#0074D9", fontface=2, size = 6)





##############################
# 9th Overlay Pitch
##############################


## create the dataframe that we will use to plot the y-axis ticks and text
df_ticks <- data.frame(x = -4, y = seq(10,40,10), xend = -2.5, yend=seq(10,40,10), label_text = as.character(seq(10,40,10)), stringsAsFactors = F )
df_ticks$label_text <- paste0(df_ticks$label_text, " ")

##
ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  ggtitle(paste0(Teams[1],": Vertical Origin of Passes (x)")) +
  theme_void() + 
  geom_histogram(data = df_player_selectT1, aes(df_player_selectT1$x), binwidth = 5, alpha = 1, fill = "#0074D9") + 
  geom_segment(aes(x = -2.5, y = -2, xend = 25, yend = -2),colour = "#435366", arrow = arrow(length = unit(0.1, "cm"), type="closed")) + 
  annotate("text", x = -2.5, y = -1, label = "Attacking Direction", colour = "#435366", fontface=2, size = 3, hjust = 0) + 
  geom_segment(data = df_ticks, aes(x = x, y=y, xend = xend, yend =yend), colour = "#435366") + 
  geom_text(data = df_ticks, aes(x = x, y=y,label = label_text), hjust=1, vjust=0.5, size = 3) +  
  annotate("text", x = 60, y = -4, label = paste0(PassTotals$player.name[1]," [",PassTotals$freq[1],"]"), colour = "#0074D9", fontface=2, size = 6) + 
  # add pitch outline
  geom_rect(aes(xmin=-2.5, xmax=122.5, ymin=0, ymax=50), fill = NA, colour = "#435366") + 
  # left hand 18 yard 
  geom_rect(aes(xmin=-2.5, xmax=16.25, ymin=11, ymax=39), fill = NA, colour = "#435366") + 
  # add right hand 18 yard 
  geom_rect(aes(xmin=103.75, xmax=122.5, ymin=11, ymax=39), fill = NA, colour = "#435366") +
  # add half way line
  geom_segment(aes(x = 60, y = 0, xend = 60, yend = 50),colour = "#435366") 






##############################
# 10th Patchwork Plots
# Plot 3 top passers
##############################


## create the player dataframe 
df_player_selectT1P1 <- df_pass_T1 %>% filter(player.name == PassTotals$player.name[1])

## create the dataframe that we will use to plot the y-axis ticks and text
df_ticks <- data.frame(x = -4, y = seq(10,40,10), xend = -2.5, yend=seq(10,40,10), label_text = as.character(seq(10,40,10)), stringsAsFactors = F )
df_ticks$label_text <- paste0(df_ticks$label_text, " ")

## plot 1
p1 <- ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  ggtitle(paste0(Teams[1],": Vertical Origin of Passes (x)")) +
  theme_void() + 
  geom_histogram(data = df_player_selectT1P1, aes(df_player_selectT1P1$x), binwidth = 5, alpha = 1, fill = "#0074D9") + 
  geom_segment(aes(x = -2.5, y = -2, xend = 25, yend = -2),colour = "#435366", arrow = arrow(length = unit(0.1, "cm"), type="closed")) + 
  annotate("text", x = -2.5, y = -1, label = "Attacking Direction", colour = "#435366", fontface=2, size = 2, hjust = 0) + 
  geom_segment(data = df_ticks, aes(x = x, y=y, xend = xend, yend =yend), colour = "#435366") + 
  geom_text(data = df_ticks, aes(x = x, y=y,label = label_text), hjust=1, vjust=0.5, size = 3) +  
  annotate("text", x = 60, y = -4, label = paste0(PassTotals$player.name[1]," [",PassTotals$freq[1],"]"), colour = "#0074D9", fontface=2, size = 6) + 
  # add pitch outline
  geom_rect(aes(xmin=-2.5, xmax=122.5, ymin=0, ymax=50), fill = NA, colour = "#435366") + 
  # left hand 18 yard 
  geom_rect(aes(xmin=-2.5, xmax=16.25, ymin=11, ymax=39), fill = NA, colour = "#435366") + 
  # add right hand 18 yard 
  geom_rect(aes(xmin=103.75, xmax=122.5, ymin=11, ymax=39), fill = NA, colour = "#435366") +
  # add half way line
  geom_segment(aes(x = 60, y = 0, xend = 60, yend = 50),colour = "#435366") 

## create the player dataframe 
df_player_selectT1P2 <- df_pass_T1 %>% filter(player.name == PassTotals$player.name[2])

## plot 2
p2 <- ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  theme_void() + 
  geom_histogram(data = df_player_selectT1P2, aes(df_player_selectT1P2$x), binwidth = 5, alpha = 1, fill = "#0074D9") + 
  geom_segment(aes(x = -2.5, y = -2, xend = 25, yend = -2),colour = "#435366", arrow = arrow(length = unit(0.1, "cm"), type="closed")) + 
  annotate("text", x = -2.5, y = -1, label = "Attacking Direction", colour = "#435366", fontface=2, size = 2, hjust = 0) + 
  geom_segment(data = df_ticks, aes(x = x, y=y, xend = xend, yend =yend), colour = "#435366") + 
  geom_text(data = df_ticks, aes(x = x, y=y,label = label_text), hjust=1, vjust=0.5, size = 3) +  
  annotate("text", x = 60, y = -4, label = paste0(PassTotals$player.name[2]," [",PassTotals$freq[2],"]"), colour = "#0074D9", fontface=2, size = 6) + 
  # add pitch outline
  geom_rect(aes(xmin=-2.5, xmax=122.5, ymin=0, ymax=50), fill = NA, colour = "#435366") + 
  # left hand 18 yard 
  geom_rect(aes(xmin=-2.5, xmax=16.25, ymin=11, ymax=39), fill = NA, colour = "#435366") + 
  # add right hand 18 yard 
  geom_rect(aes(xmin=103.75, xmax=122.5, ymin=11, ymax=39), fill = NA, colour = "#435366") +
  # add half way line
  geom_segment(aes(x = 60, y = 0, xend = 60, yend = 50),colour = "#435366") 

## create the player dataframe 
df_player_selectT1P3 <- df_pass_T1 %>% filter(player.name == PassTotals$player.name[3])

## plot 2
p3 <- ggplot(data=df_pass_T1, aes(df_pass_T1$x)) +
  geom_histogram(binwidth = 5, alpha = .7, fill = "#7FDBFF") + 
  ylim(c(-5,50)) + 
  xlim(c(-5,125)) + 
  theme_void() + 
  geom_histogram(data = df_player_selectT1P3, aes(df_player_selectT1P3$x), binwidth = 5, alpha = 1, fill = "#0074D9") + 
  geom_segment(aes(x = -2.5, y = -2, xend = 25, yend = -2),colour = "#435366", arrow = arrow(length = unit(0.1, "cm"), type="closed")) + 
  annotate("text", x = -2.5, y = -1, label = "Attacking Direction", colour = "#435366", fontface=2, size = 2, hjust = 0) + 
  geom_segment(data = df_ticks, aes(x = x, y=y, xend = xend, yend =yend), colour = "#435366") + 
  geom_text(data = df_ticks, aes(x = x, y=y,label = label_text), hjust=1, vjust=0.5, size = 3) +  
  annotate("text", x = 60, y = -4, label = paste0(PassTotals$player.name[3]," [",PassTotals$freq[3],"]"), colour = "#0074D9", fontface=2, size = 6) + 
  # add pitch outline
  geom_rect(aes(xmin=-2.5, xmax=122.5, ymin=0, ymax=50), fill = NA, colour = "#435366") + 
  # left hand 18 yard 
  geom_rect(aes(xmin=-2.5, xmax=16.25, ymin=11, ymax=39), fill = NA, colour = "#435366") + 
  # add right hand 18 yard 
  geom_rect(aes(xmin=103.75, xmax=122.5, ymin=11, ymax=39), fill = NA, colour = "#435366") +
  # add half way line
  geom_segment(aes(x = 60, y = 0, xend = 60, yend = 50),colour = "#435366")

# Join 3 Plots in 1

p1 | p2 | p3



