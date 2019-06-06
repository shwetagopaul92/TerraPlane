######## Shiny APP ########
### TerraShip ###

#'to help search, submit, monitor workflows
#'@import shiny
#'@export
runTerraShip<-function(){
  myfile = system.file("shinyApps/TerraShipApp.R",package="TerraPlane")
  source(myfile)
  TerraShip()
}