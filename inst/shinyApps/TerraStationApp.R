####################################
# Name: TerraStation
# Goal: To help begin using Terra in R.
####################################

########################
# load libraries
########################

require(DT)
require(AnVIL)
require(shiny)
require(httr)
########################

########################
# functions + setup
########################

billtab=NA
grouptab=NA

# function to fetch project names under a billing group
getProjectNames <- function(billingworkspace_name){
  ws = httr::content(terra$listWorkspaces())
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
  ws = httr::content(terra$listWorkspaces())
  mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
  myws_details = ws[mine]
  billingworkspace_name = lapply(myws_details, function(x) {x$workspace$namespace})  # options for workspace namespace
  #from this get the names avaiable for the chosen billing(workspace) group
  getProjectNames(billingworkspace_name)
}



#function to fix rows in workflow table
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

#function to fix rows in cluster table
fixCL<-function(mylistElement){
  keepCols = c(
    "autopauseThreshold", "stagingBucket",          "creator",            "googleProject",     
    "id",                          
    "dateAccessed",       "stopAfterCreation",  "status" ,           
    "clusterUrl",         "clusterName",        "operationName",     
    "googleId",           "createdDate"       
  )
  myrow=as.data.frame(mylistElement[keepCols])
  myrow
}

#function to fix rows in billing table
fixBILL<-function(mylistElement){
  keepCols = c(
    "creationStatus", "projectName",    "role"
  )
  myrow=as.data.frame(mylistElement[keepCols])
  myrow
}

# function to monitor job submissions
monitorSub <- function(workspaceNamespace, wdlnamespace){
  subDetails = httr::content(terra$listSubmissions(workspaceNamespace,wdlnamespace))
  tempDF=lapply(subDetails,fixWF)
  detailDF = do.call("rbind.data.frame",tempDF)
  detailDF
}

# function to abort jobs
abortSubmission <- function(workspaceNamespace, wdlnamespace, name){
  subDetails = httr::content(terra$listSubmissions(workspaceNamespace,wdlnamespace))
  for(detail in subDetails){
    mydetail = sapply(subDetails, function(x) {x$methodConfigurationName==name})
  }
  mytooldetail = as.data.frame(subDetails[mydetail])
  submissionId = as.character(mytooldetail$submissionId)
  abortLog = terra$abortSubmission(workspaceNamespace,wdlnamespace,submissionId)
  abortLog
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

# function to convert details list
fixMe<-function(meList){
  mydf=data.frame(matrix(unlist(meList),ncol=2,byrow=TRUE))
  names(mydf)=c("Key","Value")
  mydf
}

# function to create cluster on swagger with httr
createCluster <- function(googleProject, clusterName, jupyterDockerImage, rstudioDockerImage){
  # to get the access token 
  access=jsonlite::fromJSON(system.file(package="AnVIL",path="service/terra/auth.json"))
  app <- oauth_app(
    "Leonardo",
    key= access$client_id,
    secret = access$client_secret
  )
  token <- oauth2.0_token(
    oauth_endpoints("google"), app,
    scope= "openid email"
  )
  url=paste0("https://notebooks.firecloud.org/api/cluster/v2/",googleProject,"/",clusterName)
  httr::PUT(url=url, body=list(jupyterDockerImage=jupyterDockerImage,rstudioDockerImage=rstudioDockerImage), encode="json", httr::config(token=token))
  showNotification("Cluster Created!")
}

# function to delete a running cluster with httr
deleteCluster <- function(googleProject, clusterName){
  # to get the access token 
  access=jsonlite::fromJSON(system.file(package="AnVIL",path="service/terra/auth.json"))
  app <- oauth_app(
    "Leonardo",
    key= access$client_id,
    secret = access$client_secret
  )
  token <- oauth2.0_token(
    oauth_endpoints("google"), app,
    scope= "openid email"
  )
  url=paste0("https://notebooks.firecloud.org/api/cluster/v2/",googleProject,"/",clusterName)
  res = httr::DELETE(url=url, encode="json", httr::config(token=token))
  #showNotification("Cluster Deleted!")
  res
}


# Setup:

########################
# Shiny
########################

TerraStation = function() {
  
  curPath=NA
  mytable=NA
  # start shiny app config
  shinyApp(
    ##########
    # start UI Config
    ##########
    ui = fluidPage(
      titlePanel("TerraStation"),
      navlistPanel(widths=c(2,10),
                   tabPanel("Me",h3("Me"),
                            DT::dataTableOutput("meDetails")
                   ),
                   tabPanel("Groups",h3("Groups"),
                            DT::dataTableOutput("groupDetails"),
                            h3("GroupMembers"),
                            verbatimTextOutput("groupMembers")
                   ),
                   tabPanel("Billing",h3("Billing"),
                            DT::dataTableOutput("billingDetails"),
                            h3("Billing Account Members"),
                            DT::dataTableOutput("billingMembers")
                   ),
                   tabPanel("Create Workspace",h3("Create Workspace"),
                            uiOutput("billingwsnamespace_dropdown2"),
                            textInput("newWorkspaceName", "Workspace Name"),
                            actionButton("createWorkspace","Create Workspace")),
                   tabPanel("View Workspaces",h3("Workspaces"),
                            DT::dataTableOutput("workspaceInfo")),
                   
                   tabPanel("Monitor Workflows",h3("Monitor"),
                            uiOutput("billingwsnamespace_dropdown"),
                            uiOutput("projectnames_dropdown"),
                            DT::dataTableOutput("submissionDetails"),
                            actionButton("abortSubmission","Abort"),
                            actionButton("refreshSubmission","Refresh")),
                   tabPanel("Monitor Clusters",h3("Monitor Clusters"),
                            DT::dataTableOutput("clusterDetails"),
                            actionButton("deleteCluster","Delete")),
                   tabPanel("Create Cluster", h3("Create Cluster"),
                            textInput("googleProject", "Google Project Name"),
                            textInput("clusterName", "Cluster Name"),
                            textInput("jupyterDockerImage", "Jupyter Docker Image"),
                            textInput("rstudioDockerImage", "RStudio Docker Image"),
                            actionButton("createCluster","Create Cluster"),
                            actionButton("deleteCluster","Delete Cluster")),
                   tabPanel("About", h3("About"), HTML('<br> TerraStation is a shiny interface to help begin using Terra in R <br>'))
      )
    ),
    ####################
    # Start Server Config
    ####################
    server = function(input, output, session) {
      
      # dropdown for billing groups a user belongs to
      output$billingwsnamespace_dropdown <- renderUI({
        ws = httr::content(terra$listWorkspaces())
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
      
      # dropdown for billing groups a user belongs to
      output$billingwsnamespace_dropdown2 <- renderUI({
        ws = httr::content(terra$listWorkspaces())
        mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
        myws_details = ws[mine]
        # options for workspace namespace
        workspacename = lapply(myws_details, function(x) {x$workspace$namespace})
        # from this get the names avaiable for the chosen billing(workspace) group
        selectInput("workspaceNamespace2",
                    "Select Workspace Namespace",
                    choices = workspacename
        )
      })
      
      # dropdown for project names available under a billing group
      output$projectnames_dropdown <- renderUI({
        ws = httr::content(terra$listWorkspaces())
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
        ws_tool_names = httr::content(ws)
        all_tool_names = lapply(ws_tool_names, function(x) {x$name})
        selectInput("name",
                    "Select Tool",
                    choices = all_tool_names
        )
      })
      
      # populate Me Table
      output$meDetails = DT::renderDataTable(fixMe(httr::content(terra$getAll())[[2]]))
      
      # populate Billing Table
      billtab<<-do.call("rbind.data.frame",lapply(httr::content(terra$billing()), fixBILL))
      
      output$billingDetails = DT::renderDataTable(billtab, selection = 'single')
      
      # populate billing memberships
      output$billingMembers = DT::renderDataTable({
        s = input$billingDetails_rows_selected
        if (length(s)) {
          do.call("rbind.data.frame", httr::content(terra$listBillingProjectMembers(billtab$projectName[s])))
        }
      })
      
      # populate Groups Table
      grouptab<<-do.call("rbind.data.frame", httr::content(terra$getGroups()))
      output$groupDetails = DT::renderDataTable(grouptab, selection = 'single')
      
      # populate Group memberships
      output$groupMembers=renderPrint({
        s = input$groupDetails_rows_selected
        if (length(s)) {
          str(httr::content(terra$getGroup(grouptab$groupName[s])))
          
        }
      })
      
      # populate workspace table
      myworkspaces = httr::content(terra$listWorkspaces())
      parseWorkspace<-function(listElement){
        res = list()
        res$accessLevel = listElement[1]$accessLevel
        res$public = listElement[2]$public
        wp = listElement[3]$workspace
        res$name = wp$name
        res$namespace = wp$namespace
        res$bucketName = wp$bucketName
        res$createdBy = wp$createdBy
        res$createdDate = wp$createdDate
        res$lastModified = wp$lastModified
        res$workspaceId = wp$workspaceId
        res
      }
      
      output$workspaceInfo=DT::renderDataTable(do.call("rbind.data.frame",lapply(myworkspaces,parseWorkspace)))
      
      # populate cluster info
      clusters = httr::content(leonardo$listClusters())
      tempDFCL = lapply(clusters,fixCL)
      clDetailDF = do.call("rbind.data.frame",tempDFCL)
      output$clusterDetails = renderDT({clDetailDF})
      
      # create Workspace
      observeEvent(input$createWorkspace, {
        a = terra$createWorkspace(input$workspaceNamespace2,input$newWorkspaceName, attributes=AnVIL::empty_object)
      })
      
      # monitor job submission
      output$submissionDetails = DT::renderDataTable(monitorSub(input$workspaceNamespace, input$wdlnamespace))
      
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
                                                                  input$name))
      })
      
      # observe event create cluster
      observeEvent(input$createCluster, {
        createCluster(input$googleProject, input$clusterName, input$jupyterDockerImage, input$rstudioDockerImage)
      })
      
      # observer event delete cluster
      observeEvent(input$deleteCluster, {
        res = deleteCluster(input$googleProject, input$clusterName)
        if(res$status_code != 200){
          showNotification("Deleting!")
        }
        else{
          showNotification("Deleted!")
        }
      })
    }
)}
