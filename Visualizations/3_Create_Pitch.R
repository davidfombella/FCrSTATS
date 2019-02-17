
options(warn=-1)

require(ggplot2)

createPitch <- function(xmax, ymax, grass_colour, line_colour, background_colour, goal_colour){
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  ymin <- 0 
  xmin <- 0 
  
  # Defining dimensions
  GoalWidth <- 732
  penspot <- 1100
  boxedgeW <- 4032
  boxedgeL <- 1650
  box6yardW <- 1832
  box6yardL <- 550
  
  ## dimensions calculations 
  # The 18 Yard Box
  TheBoxWidth <- c(((ymax / 2) + (boxedgeW / 2)),((ymax / 2) - (boxedgeW / 2)))
  TheBoxHeight <- c(boxedgeL,xmax-boxedgeL)
  GoalPosts <- c(((ymax / 2) + (GoalWidth / 2)),((ymax / 2) - (GoalWidth / 2)))
  
  # The 6 Yard Box
  box6yardWidth <- c(((ymax / 2) + (box6yardW / 2)),((ymax / 2) - (box6yardW / 2)))
  box6yardHeight <- c(box6yardL,xmax-box6yardL)
  
  ## Centre circle dimensions 
  centreCirle_d <- 1830
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### create leftD arc ####
  Dleft <- circleFun(c((penspot),(ymax/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]
  
  ## create rightD arc  ####
  Dright <- circleFun(c((xmax-(penspot)),(ymax/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dright <- Dright[which(Dright$x <= (xmax-(boxedgeL))),]
  
  #### create center circle ####
  center_circle <- circleFun(c((xmax/2),(ymax/2)),centreCirle_d,npoints = 100)
  
  ## create corner flag radius ####
  TopLeftCorner <- circleFun(c(xmin,ymax),200,npoints = 1000)
  TopRightCorner <- circleFun(c(xmax,ymax),200,npoints = 1000)
  BottomLeftCorner <- circleFun(c(xmin,ymin),200,npoints = 1000)
  BottomRightCorner <- circleFun(c(xmax,ymin),200,npoints = 1000)
  
  p <- ggplot() + xlim(c(-10,xmax+10)) + ylim(c(-10,ymax+10)) + 
    # add the theme 
    theme_blankPitch() +
    # add the base rectangle of the pitch 
    geom_rect(aes(xmin=0, xmax=xmax, ymin=0, ymax=ymax), fill = grass_colour, colour = line_colour) +
    # add the 18 yard box Left
    geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour) + 
    # add the 18 yard box Right
    geom_rect(aes(xmin=TheBoxHeight[2], xmax=xmax, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = grass_colour, colour = line_colour) +
    # add the six yard box Left
    geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)  +
    # add the six yard box Right
    geom_rect(aes(xmin=box6yardHeight[2], xmax=xmax, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = grass_colour, colour = line_colour)  + 
    # Add half way line 
    geom_segment(aes(x = xmax/2, y = ymin, xend = xmax/2, yend = ymax),colour = line_colour) +
    # add left D 
    geom_path(data=Dleft, aes(x=x,y=y), colour = line_colour) + 
    # add Right D 
    geom_path(data=Dright, aes(x=x,y=y), colour = line_colour) + 
    # add centre circle 
    geom_path(data=center_circle, aes(x=x,y=y), colour = line_colour) + 
    # add penalty spot left 
    geom_point(aes(x = penspot , y = ymax/2), colour = line_colour) + 
    # add penalty spot right
    geom_point(aes(x = (xmax-(penspot)) , y = ymax/2), colour = line_colour) + 
    # add centre spot 
    geom_point(aes(x = (xmax/2) , y = ymax/2), colour = line_colour) +
    # add Corner Flag corners
    geom_path(data=TopLeftCorner, aes(x=x,y=y), colour = line_colour) +
    geom_path(data=TopRightCorner, aes(x=x,y=y), colour = line_colour) +
    geom_path(data=BottomLeftCorner, aes(x=x,y=y), colour = line_colour) +
    geom_path(data=BottomRightCorner, aes(x=x,y=y), colour = line_colour) +
    geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = goal_colour, size = 1) +
    # add the goal right
    geom_segment(aes(x = xmax, y = GoalPosts[1], xend = xmax, yend = GoalPosts[2]),colour = goal_colour, size = 1) 
  return(p)
}



## you can define the values first like below
grass_colour <- "#775D6A"
line_colour <- "#F4828C"
background_colour <- "#775D6A"
goal_colour <- "#7E3C5D"
ymax <- 7040
xmax <- 10600

createPitch(xmax, ymax, grass_colour, line_colour, background_colour, goal_colour)

## green 
createPitch(10600, 7040, "#538032", "#ffffff", "#538032", "#000000")


## Blue 
createPitch(10600, 7040, "#224C56", "#B3CED9", "#224C56", "#15393D")





