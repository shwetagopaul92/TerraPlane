####################################
# Name: TerraShip
# Goal: To help search, submit, monitor workflows on Terra.
####################################

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

# function to get tool names for the specified project
getMethodConfigNames <- function(billingworkspace_name, project_name){
  ws = terra$listWorkspaceMethodConfigs(workspaceNamespace = billingworkspace_name,
                                        workspaceName = project_name,allRepos = TRUE)
  ws_tool_names = content(ws)
  ws_tool_names
  
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

# fix method details
fixWF<-function(mylistElement){
  myrow=as.data.frame(mylistElement)
  if(!("submissionEntity.entityType"%in%names(myrow))){
    myrow[["submissionEntity.entityType"]]=NA
    myrow[["submissionEntity.entityName"]]=NA
  }
  keepCols = c("methodConfigurationName","methodConfigurationNamespace", "submissionEntity.entityType","submissionEntity.entityName",
               "status","submissionDate","submissionId","submitter","useCallCache")
  
  myrow[,keepCols]
}

# function to monitor job submissions
monitorSub <- function(workspaceNamespace, wdlnamespace, name){
  subDetails = content(terra$listSubmissions(workspaceNamespace,wdlnamespace))
  tempDF=lapply(subDetails,fixWF)
  detailDF = do.call("rbind.data.frame",tempDF)
  bucketName = getBucketName(workspaceNamespace, wdlnamespace)
  link = paste0("https://console.cloud.google.com/storage/browser/",bucketName)
  detailDF$bucketName = paste0("<a href='",link,"'>",bucketName,"</a>")
  #keepCols = c("methodConfigurationName","methodConfigurationNamespace","status","submissionDate","submissionId","submitter","useCallCache","bucketName")
  detailDF[detailDF$methodConfigurationName==name,]
}

# function to abort jobs 
abortSubmission <- function(workspaceNamespace, wdlnamespace, name){
  subDetails = content(terra$listSubmissions(workspaceNamespace,wdlnamespace))
  for(detail in subDetails){
    mydetail = sapply(subDetails, function(x) {x$methodConfigurationName==name})
  }
  mytooldetail = as.data.frame(subDetails[mydetail])
  submissionId = as.character(mytooldetail$submissionId)
  abortLog = terra$abortSubmission(workspaceNamespace,wdlnamespace,submissionId)
  abortLog
}

# get BucketName for viewing results link
getBucketName <- function(workspaceNamespace, wdlnamespace){
  ws = content(terra$listWorkspaces())
  mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
  myws_details = ws[mine]
  bucketList = lapply(myws_details, function(x) {
    if(x$workspace$namespace==workspaceNamespace & x$workspace$name==wdlnamespace){
      x$workspace$bucketName
    }
  })
  bucketName = as.character(bucketList[-which(sapply(bucketList, is.null))])
  bucketName
}

# function to convert tool details list to a dataframe
list_to_df <- function(listfordf){
  if(!is.list(listfordf)) stop("it should be a list")
  df <- list(list.element = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))
  
  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }
  df
}

# function to print list elements as a bullets
listElementBullet<-function(myListElement, myListElementName){
  myhtml=paste0(myListElementName,"<ul>")
  if(length(myListElement)>0){
    for(i in 1:length(myListElement)){
      myhtml=paste0(myhtml,"<li>",myListElement[[i]],"</li>")
    }
  }
  myhtml=paste0(myhtml,"</ul>")
  return(myhtml)
}

# function to print list elements as a bullets
listBullet<-function(mylist,mylistnames){
  myhtml=""
  for(i in 1:length(mylistnames)){
    myhtml=paste0(myhtml,listElementBullet(mylist[[i]],mylistnames[i]))
  }
  return(myhtml)
}


# Setup:

########################
# Shiny
########################

TerraShip = function() {
  
  curPath=NA
  mytable=NA
  # start shiny app config
  shinyApp(
    ##########
    # start UI Config
    ##########
    ui = fluidPage(
      titlePanel("TerraShip"),
      sidebarLayout(position = "left",
                    sidebarPanel(width=2,
                                 uiOutput("billingwsnamespace_dropdown"),
                                 uiOutput("projectnames_dropdown"),
                                 uiOutput("toolnames_dropdown"),
                                 actionButton("submitButton", "Submit")
                    ),
                    mainPanel("",width=10,
                              (tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }")),
                              tabsetPanel(
                                tabPanel("Tool Configuration",h3("Configuration"),
                                         verbatimTextOutput("tooldetails"),
                                         actionButton("runOnTerra","Run Analysis")),
                                tabPanel("Monitor Workflow",h3("Monitor"),
                                         DT::dataTableOutput("submissionDetails"),
                                         actionButton("abortSubmission","Abort"),
                                         actionButton("refreshSubmission","Refresh")),
                                tabPanel("About", h3("About"), HTML('<br> TerraShip is a shiny interface to help search, submit, monitor workflows on Terra. 
                                                                    <br>')
                                )
                                )
                              )
                    )
      ),
    ####################
    # Start Server Config
    ####################
    server = function(input, output, session) {
      
      # dropdown for billing groups a user belongs to
      output$billingwsnamespace_dropdown <- renderUI({
        ws = content(terra$listWorkspaces())
        mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
        myws_details = ws[mine]
        # options for workspace namespace
        workspacename = lapply(myws_details, function(x) {x$workspace$namespace})
        # from this get the names avaiable for the chosen billing(workspace) group
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
      
      # dropdown for tools names available under a billing group
      output$toolnames_dropdown <- renderUI({
        ws = terra$listWorkspaceMethodConfigs(workspaceNamespace = input$workspaceNamespace,
                                              workspaceName = input$wdlnamespace,allRepos = TRUE)
        ws_tool_names = content(ws)
        all_tool_names = lapply(ws_tool_names, function(x) {x$name})
        selectInput("name", 
                    "Select Tool",
                    choices = all_tool_names
        )
      })
      
      # show selected tool's details
      observeEvent(input$submitButton, {
        details = terra$getWorkspaceMethodConfig(input$workspaceNamespace,input$wdlnamespace
                                                 ,input$wdlnamespace, input$name)
        tooldetails = content(details)
        #output$tooldetails = renderUI(HTML(listBullet(tooldetails,names(tooldetails))))
        
        #df = list_to_df(tooldetails)
        #names(df) = c("Value","Name")
        #reordered_df = df[, c(2,1)]
        output$tooldetails = renderPrint(str(tooldetails))
      })
      
      
      # create and submit jobs 
      observeEvent(input$runOnTerra, {
        terra$createSubmission(
          workspaceNamespace=input$workspaceNamespace,
          workspaceName=input$wdlnamespace,
          methodConfigurationNamespace=input$wdlnamespace,
          methodConfigurationName=input$name,
          useCallCache=TRUE)
        showNotification("Job created!")
      })
      
      # monitor job submission
      output$submissionDetails = DT::renderDataTable(monitorSub(input$workspaceNamespace, input$wdlnamespace,
                                                                input$name), escape=FALSE)
      
      # abort job submission
      observeEvent(input$abortSubmission, {
        res = abortSubmission(input$workspaceNamespace, input$wdlnamespace, input$name)
        if(res$status_code != 200){
          showNotification("Aborting!")
        }
        else{
          showNotification("Aborted!")
        }
      })
      
      # refresh monitor tab to show updated details
      observeEvent(input$refreshSubmission, {
        output$submissionDetails = DT::renderDataTable(monitorSub(input$workspaceNamespace, input$wdlnamespace,
                                                                  input$name), escape=FALSE)
      })
      
      
    }
    )}
