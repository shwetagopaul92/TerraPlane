######## Shiny APP ########
### terraplane ###

#'to help filter dockstore to find methods
#'@import shiny
#'@export
runTerraPlane<-function(){
  myfile = system.file("shinyApps/TerraPlaneApp.R",package="TerraPlane")
  source(myfile)
  TerraPlane()
}