####################################
# Name: TerraPlane
# Goal: To help filter dockstore to find methods
#      based on search term
#####################################

########################
# load libraries
########################
require(shiny)
require(DT)
require(AnVIL)
########################


########################
# functions + setup
########################

ids=c()
wdlinputs=c()
wdlinputNames=c()
wdlinputNamesUnc=c()
wdlpath="";
wdlversion=1;
wdlconfig="";
wdlinputsList=list()
outputs = list()
prerequisites= list()
hasSamp=0
TerraLogs=""

# function to filter/search for Dockstore workflows based on user input
filterDS<-function(mypattern){
  temp=jsonlite::fromJSON(
    httr::content(dockstore$allPublishedWorkflows(filter=mypattern),"text"))
  #validate(
  # need(is.data.frame(temp), "Please choose a valid pattern!")
  #)
  keep=c("workflowName","description","author","organization","id","path",
         "full_workflow_path","gitUrl","sourceControl","dbCreateDate","last_modified_date")
  temp[,keep]
}

# function to fetch WDL of a chosen method from Dockstore
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

# function to parse inputs
parseInputs<-function(inputList){
  inputNames=names(inputList)
  inputNames=sapply(inputNames,function(x){
    temp=strsplit(x, split="[.]")[[1]]
    paste0(temp[1],".",temp[length(temp)])
  })
  wdlinputNamesUnc<<-inputNames
  inputNames
}

# function to clear inputs
clearInputs <- function(){
  for(i in 1:length(ids)){
    removeUI(
      selector = paste0('#', ids[i])
    )
  }
  ids<<-c()
}

# function to create inputs
# https://gallery.shinyapps.io/111-insert-ui/
createInputs<-function(inputVector){
  clearInputs()
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

# function to fetch project names under a billing group
getProjectNames <- function(billingworkspace_name){
  ws = content(terra$listWorkspaces())
  mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
  myws_details = ws[mine]
  for(i in myws_details){
    mybilling = sapply(myws_details, function(x) {x$workspace$namespace==billingworkspace_name})
  }
  myProjectName = myws_details[mybilling]
  myProjectName
  
}

# function to fetch billing groups a user belongs to
getBillingWorkspace <- function(){
  ws = content(terra$listWorkspaces())
  mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
  myws_details = ws[mine]
  billingworkspace_name = lapply(myws_details, function(x) {x$workspace$namespace})  # options for workspace namespace
  #from this get the names avaiable for the chosen billing(workspace) group
  getProjectNames(billingworkspace_name)
}

# Setup:

########################
# Shiny
########################

TerraPlane = function() {
  
  curPath=NA
  mytable=NA
  # start shiny app config
  shinyApp(
    ##########
    # start UI Config
    ##########
    ui = fluidPage(
      titlePanel("TerraPlane"),
      sidebarLayout(position = "left",
                    sidebarPanel(width=2,
                                 textInput("filterPattern", "Filter Pattern"),
                                 actionButton("filterButton", "Filter"),
                                 actionButton("resetButton", "Reset")
                    ),
                    mainPanel("",width=10,
                              (tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }")),
                              tabsetPanel(
                                tabPanel("Data",h3("Current Data"),
                                         DT::dataTableOutput('mytable')),
                                tabPanel("Method",h3("Method"),
                                         htmlOutput("methodcode")),
                                tabPanel("Configure",h3("Configure Method"),
                                         uiOutput("billingwsnamespace_dropdown"),
                                         uiOutput("projectnames_dropdown"),
                                         textInput("wdlname", "Name"),
                                         textInput("outputs", "Outputs"),
                                         shiny::tags$div(id = 'placeholder'),
                                         actionButton("sendToTerra", "Send To Terra"),
                                         actionButton("runOnTerra", "Run")
                                ),
                                tabPanel("About", h3("About"), HTML('<br> Terraplane is a shiny interface to help filter and configure dockstore methods
                                                                    based on search term. <br>')
                                )
                                )
                              )
                    )
      ),
    ####################
    # Start Server Config
    ####################
    server = function(input, output, session) {
      # set up output
      observeEvent(input$filterButton, {
        subs = tryCatch(filterDS(input$filterPattern), 
                        error=function(e) print("Please enter a valid pattern!"))
        mytable<<-subs
        if (is.data.frame(mytable)){
          output$mytable = DT::renderDataTable(subs,options = list(scrollX = TRUE,pageLength=5),selection = 'single')
        }
        else{
          subs = as.data.frame(subs)
          names(subs) <- NULL
          output$mytable = DT::renderDataTable(subs)
        }
      })
      
      # dropdown for billing groups a user belongs to
      output$billingwsnamespace_dropdown <- renderUI({
        ws = content(terra$listWorkspaces())
        mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
        myws_details = ws[mine]
        # options for workspace namespace
        workspace_name = lapply(myws_details, function(x) {x$workspace$namespace})  
        # from this get the names avaiable for the chosen billing(workspace) group
        workspacename = as.list(workspace_name)
        selectInput("workspaceNamespace", 
                    "Select Workspace Namespace",
                    choices = workspacename
        )
      })
      
      # dropdown for project names available under a billing group
      output$projectnames_dropdown <- renderUI({
        ws = content(terra$listWorkspaces())
        mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
        myws_details = ws[mine]
        for(i in myws_details){
          mybilling = sapply(myws_details, function(x) {x$workspace$namespace==input$workspaceNamespace})
        }
        myProjectName = myws_details[mybilling]
        project_names = lapply(myProjectName, function(x) {x$workspace$name})
        myProjectNames = as.list(project_names)
        selectInput("wdlnamespace", 
                    "Select Project Name",
                    choices = myProjectNames
        )
      })
      
      # display the method code 
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
      
      # allow users to reset configuration
      observeEvent(input$resetButton, {
        subs=data.frame(id=NA,workflow=NA)
        output$mytable = DT::renderDataTable({subs},options = list(scrollX = TRUE,pageLength=5))
        clearInputs()
      })
      
      # clear inputs
      clearInputs <- function(){
        for(i in 1:length(ids)){
          removeUI(
            selector = paste0('#', ids[i])
          )
        }
        ids<<-c()
      }
      
      # send defined configuration to Terra
      observeEvent(input$sendToTerra, {
        myinputs=sapply(wdlinputNames, function(x){
          input[[x]]
        })
        names(myinputs)=wdlinputNamesUnc
        wdlInputsList<<-as.list(myinputs)
        TerraLogs<<-terra$postWorkspaceMethodConfig(
          workspaceNamespace=input$workspaceNamespace,
          workspaceName=input$wdlnamespace,
          namespace=input$wdlnamespace,
          name=input$wdlname,
          rootEntityType=NA,
          inputs=wdlInputsList,
          outputs=AnVIL::empty_object,
          prerequisites=AnVIL::empty_object,
          methodRepoMethod=list(
            sourceRepo="dockstore",
            methodPath=wdlpath,
            methodVersion=wdlversion
          ),
          methodConfigVersion=0,
          deleted=FALSE)
        showNotification("Method posted on Terra!")
      })
      
      # create and submit jobs on Terra
      observeEvent(input$runOnTerra, {
        terra$createSubmission(
          workspaceNamespace=input$workspaceNamespace,
          workspaceName=input$wdlnamespace,
          methodConfigurationNamespace=input$wdlnamespace,
          methodConfigurationName=input$wdlname,
          useCallCache=TRUE)
        showNotification("Job created!")
      })
    })
}