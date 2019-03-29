######## Shiny APP ########
### terraplane ###

#'to help filter dockstore to find methods
#'@import shiny
#'@export
runterraplane<-function(){
  myfile = system.file("shinyApps/terraplaneApp.R",package="terraplane")
  source(myfile)
  terraplane()
}