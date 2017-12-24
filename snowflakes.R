

library(snowflakes)
library(randomcoloR)

# https://cran.r-project.org/web/packages/snowflakes/snowflakes.pdf
t = seq(0, 5*pi, .5)
xCoor = t*cos(t)
yCoor = t*sin(t)
radius = 1
orientation = runif(length(xCoor))*(pi/6)
set.seed(1)
plot(xCoor, yCoor, type="l", axes = TRUE, ylab="", xlab="",
     ylim = range(yCoor) + radius*c(-1, 1)*3,
     xlim = range(xCoor) + radius*c(-1, 1)*0, col=gray(.9))
returnedSeeds = snowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius,
                           orientation = orientation, seeds = 1:3,
                           color = gray((1:length(xCoor))/(length(xCoor)+1)), anotherColor = "gray")


# https://cran.r-project.org/web/packages/snowflakes/vignettes/snowflakes.html
xCoor = seq(0, 2, .25)
yCoor = (xCoor-1)^2
radius = 0.1
set.seed(1)
par(mar = c(0, 0, 0, 0))
plot(xCoor, yCoor, type="l", axes = FALSE, ylab="", xlab="", ylim = range(yCoor) + radius*c(-1, 1)*0.7, xlim = range(xCoor) + radius*c(-1, 1)*0.7, col=gray(.9))
snowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius, color = "#22222222")




# https://cran.r-project.org/web/packages/snowflakes/vignettes/snowflakes.html
for (i in 1:10) {
  xCoor = seq(0, 2, .5)
  yCoor = (xCoor-1)^2
  radius = 0.1
  set.seed(1)
  par(mar = c(0, 0, 0, 0))
  plot(xCoor, yCoor, type="l", axes = FALSE, ylab="", xlab="", ylim = range(yCoor) + radius*c(-1, 1)*0.7, xlim = range(xCoor) + radius*c(-1, 1)*0.7, col=gray(.9))
  snowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius, color = "#22222222")
  print(i)
  Sys.sleep(1)
}


# https://cran.r-project.org/web/packages/randomcoloR/randomcoloR.pdf
# Simple function for random color
snowflake_rainbow <- function() 
{
  return(randomColor(count = 1, hue = c(" ", "random", "red", "orange", "yellow","green", "blue", "purple", "pink", "monochrome"), luminosity = c(" ","random", "light", "bright", "dark")))
}
#test
snowflake_rainbow()


# BAsic Loop with COlor Snowflakes
for (i in 1:10) {
  set.seed(i)
  xCoor = runif(1)*2
  yCoor = runif(1)*2
  radius = .2
  plot(xCoor, yCoor, type="l", axes = FALSE, ylab="", xlab="", ylim = c(-.5, 2.5), xlim = c(-.5, 2.5), col=gray(.9))
  snowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius, color = snowflake_rainbow())
  returnedSeeds = snowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius, seeds = i, deltaCoef = 15 - (0:(length(xCoor)-1))*3, color = snowflake_rainbow())
  print(i)
  print(xCoor)
  print(yCoor)
  Sys.sleep(.5)
  }


# with some color
t = seq(0, 5*pi, .5)
xCoor = t*cos(t)
yCoor = t*sin(t)
radius = 1
orientation = runif(length(xCoor))*(pi/6)
set.seed(1)
plot(xCoor, yCoor, type="l", axes = TRUE, ylab="", xlab="",
     ylim = range(yCoor) + radius*c(-1, 1)*3,
     xlim = range(xCoor) + radius*c(-1, 1)*0, col=gray(.9))
returnedSeeds = snowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius,
                           orientation = orientation, seeds = 1:3,
                           color = snowflake_rainbow(), anotherColor = "gray")




# https://cran.r-project.org/web/packages/snowflakes/vignettes/snowflakes.html

fancySnowflakes = function(xCoor, yCoor, radius, seeds, orientation = pi/6){
  colorCoord = as.hexmode(col2rgb("blue"))
  transpBlue =paste("#", paste(colorCoord, collapse=""), "44", sep="")
  colorCoord = as.hexmode(col2rgb("white"))
  transpWhite =paste("#", paste(colorCoord, collapse=""), "66", sep="")
  for (i in 1:length(xCoor)){
    snowflakes(xCoor = xCoor[i], yCoor = yCoor[i], radius = radius, color = transpBlue, anotherColor = "#33333333", seeds = seeds[i])
    snowflakes(xCoor = xCoor[i], yCoor = yCoor[i], radius = radius, color = transpWhite, anotherColor = transpWhite, seeds = seeds[i])
  }
}


numSnowflPerRow = 5
set.seed(2018)
selectSeeds = sample(1:100000, numSnowflPerRow^2)
xCoor = rep(1:numSnowflPerRow, numSnowflPerRow)
yCoor = rep(1:numSnowflPerRow, each = numSnowflPerRow)
radius = .4
par(mar = c(0, 0, 0, 0))
plot(range(xCoor)+c(-1, 1)*.5, range(yCoor)+c(-1, 1)*.5, type="n", axes = FALSE, ylab="", xlab="")
selectSeeds = c(90068, 46258, 41165, 66142, 4636, 37906, 17295, 9250, 30595, 74555, 12669, 62970, 96997, 43447, 81975, 23841, 73197, 8419, 14318, 83885, 29343, 47445, 66721, 29854, 77912)
fancySnowflakes(xCoor = xCoor, yCoor = yCoor, radius = radius, seeds = selectSeeds)
text(xCoor, yCoor, selectSeeds, cex=.5, pos = 3)


