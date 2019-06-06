######## Shiny APP ########
### TerraStation ###

#'to To help begin using Terra in R
#'@import shiny
#'@export
runTerraStation<-function(){
  myfile = system.file("shinyApps/TerraStationApp.R",package="TerraPlane")
  source(myfile)
  TerraStation()
}