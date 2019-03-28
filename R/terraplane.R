####################################
#Name: terraplane
#Goal: To help filter dockstore to find methods
#      based on search term
#
####################################

########################
#load libraries
########################
require(shiny)
require(shinyalert)
require(DT)
require(AnVIL)
########################


########################
#functions + setup
########################

ids=c()
wdlinputs=c()
wdlinputNames=c()
wdlinputNamesUnc=c()
wdlpath="";
wdlversion=1;
wdlconfig="";
wdlinputsList=list()
hasSamp=0

filterDS<-function(mypattern){
  temp=jsonlite::fromJSON(
    httr::content(dockstore$allPublishedWorkflows(filter=mypattern),"text"))
  #validate(
   # need(is.data.frame(temp), "Please choose a valid pattern!")
  #)
  keep=c("workflowName","description","author","organization","id","path",
  "full_workflow_path","gitUrl","sourceControl","dbCreateDate","last_modified_date")
  temp[,keep]}

getWDL<-function(path){
  wdlpath<<-URLencode(path,reserved=TRUE)
  temp=jsonlite::fromJSON(
    httr::content(dockstore$getPublishedWorkflowByPath(wdlpath), "text"))
  wdlversion<<-temp$workflowVersions$name[length(temp$workflowVersions$name)]
  temp2=temp$workflowVersions$sourceFiles
  temp2=temp2[[length(temp2)]]
  wdl=temp2[temp2$type=="DOCKSTORE_WDL","content"]
  sampleInputs="{}";
  if(any(temp2$type=="WDL_TEST_JSON")){
  	sampleInputs=temp2[temp2$type=="WDL_TEST_JSON","content"]
        hasSamp<<-1
  }
  return(list(wdl=wdl, sampleInputs=sampleInputs))
}

parseInputs<-function(inputList){
	inputNames=names(inputList)
	inputNames=sapply(inputNames,function(x){
		temp=strsplit(x, split="[.]")[[1]]
		paste0(temp[1],".",temp[length(temp)])
	})
	wdlinputNamesUnc<<-inputNames
	inputNames
}


#https://gallery.shinyapps.io/111-insert-ui/
createInputs<-function(inputVector){
  for(i in 1:length(inputVector)){
	id=paste0("div",length(ids))
	textname=gsub(inputVector[i],pattern="[.]",replacement="_")
  	insertUI(
      		selector = '#placeholder',
      		## wrap element in a div with id for ease of removal
      		ui = shiny::tags$div(
			textInput(textname,inputVector[i]),
       	 		id = id
        		)
		)
	ids<<-c(ids,id)
	wdlinputNames<<-c(wdlinputNames,textname)
   }
    	
}


createConfig<-function(wdlnamespace, wdlname, wdlInputsList){

	temp=list(
		namespace=wdlnamespace,
		name=wdlname,
		rootEntityType="string",
		inputs=wdlInputsList,
		outputs=NULL,
		prerequisites=NULL,
		methodRepoMethod=list(
			sourceRepo="dockstore",
			methodPath=wdlpath,
			methodVersion=wdlversion
		),
		methodConfigVersion=0
	)
	jsonlite::toJSON(temp, auto_unbox =TRUE, null="list")
}



#Setup:

########################
#shiny
########################
terraplane = function() {


########################
curPath=NA
mytable=NA
  #start shiny app config
  shinyApp(
  ##########
  #Start UI Config
  ##########
    ui = fluidPage(
      titlePanel("terraplane"),
      sidebarLayout(position = "left",
        sidebarPanel(width=2,
          textInput("filterPattern", "Filter Pattern"),
          actionButton("filterButton", "Filter"),
          actionButton("resetButton", "Reset")
          ),
        mainPanel("",width=10,
          tabsetPanel(
            tabPanel("Data",h3("Current Data"),
            dataTableOutput('mytable')),
            tabPanel("Method",h3("Method"),
            htmlOutput("methodcode")),
            tabPanel("Configure",h3("Configure Method"),
	        actionButton("configureButton", "Configure"),
          	textInput("workspaceNamespace", "Workspace Namespace"),
          	textInput("wdlnamespace", "Namespace"),
          	textInput("wdlname", "Name"),
		shiny::tags$div(id = 'placeholder') 
	    ),
	    tabPanel("CurrentConfig",h3("Method"),
	        actionButton("sendToTerra", "SendToTerra"),
          	textOutput("currentconfig"))
          )
        )
      )
    )
  ,
  ####################
  #Start Server Config
  ####################
  server = function(input, output, session) {
    #set up output
    observeEvent(input$filterButton, {
      subs = tryCatch(filterDS(input$filterPattern), 
                      error=function(e) print("Please enter a valid pattern!"))
      mytable<<-subs
      if (is.data.frame(mytable)){
        output$mytable = renderDataTable(subs,options = list(scrollX = TRUE,pageLength=5),selection = 'single')
      }
      else{
        subs = as.data.frame(subs)
        names(subs) <- NULL
        output$mytable = renderDataTable(subs)
      }
    })

    output$methodcode=renderText({
      mytext="No Method Selected"
      s = input$mytable_rows_selected
      if (length(s)) {
        curPath<<-mytable[s,"full_workflow_path"]
        if(!is.na(curPath)){
          myres=getWDL(curPath)
	  wdl=myres[["wdl"]]
          mytext=gsub("\\n","<br>",wdl)
	  if(hasSamp){
          	wdlinputs<<-parseInputs(jsonlite::fromJSON(myres[["sampleInputs"]]))
	  	createInputs(wdlinputs)
	  }
         }
      }
      mytext
    })

    observeEvent(input$resetButton, {
      subs=data.frame(id=NA,workflow=NA)
      output$mytable = renderDataTable({subs},options = list(scrollX = TRUE,pageLength=5))
    })

    observeEvent(input$sendToTerra, {
      terra$postWorkspaceMethodConfig(
		workspaceNamespace=input$workspaceNamespace,
		workspaceName=input$wdlnamespace,
		namespace=input$wdlnamespace,
		name=input$wdlname,
		rootEntityType="string",
		inputs=wdlInputsList,
		outputs=list(a="A"),
		prerequisites=list(a="A"),
		methodRepoMethod=list(
			sourceRepo="dockstore",
			methodPath=wdlpath,
			methodVersion=wdlversion
		),
		methodConfigVersion=0,
		deleted=FALSE)
    showNotification("Method posted on Terra!")
    })

    observeEvent(input$configureButton, {
	myinputs=sapply(wdlinputNames, function(x){
		input[[x]]
	})
	names(myinputs)=wdlinputNamesUnc
	wdlInputsList<<-as.list(myinputs)
	wdlconfig<<-createConfig(input$wdlnamespace, input$wdlname, wdlInputsList )
      	output$currentconfig=renderText(wdlconfig)
    #showNotification("Configured!")
    shinyalert("Yay!","Method configured!", type="success")
    })
  }
)
}
