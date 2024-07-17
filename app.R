##
#' @author - Chad Burris
#' @description - R Shiny for Calculating Risk of Re-identification and 
#'                  Information Loss
#' @aliases https://connect-insight.rda.onetakeda.com/anon_risk_loss/
##

library(shiny)
# library(ssh)
library(shinyFiles)
library(DT)
library(haven)
library(formattable)
library(RColorBrewer)
library(dplyr)
library(purrr)
library(shinyjs)
library(openxlsx)
# library(RColorBrewer)
library(ggplot2)
library(stringr)

ui <- fluidPage(
  useShinyjs(),
  navbarPage("Anonymization Risk and Loss",
             tabsetPanel(type='tabs',id='mainTabSet',
                         tabPanel("Setup",
                                  sidebarPanel(width=3,
                                               h4("Upload Dataset Containing Variables to be Anonymized (e.g. ADSL)"),HTML("<br/>"),
                                               fileInput('inDemogFile',"Choose Dataset",placeholder='C:\\'),
                                               hidden(selectInput('studyid','Variable(s) to use for STUDYID:',NULL,multiple=T)),
                                               hidden(selectInput('usubjid','Variable(s) to use for USUBJID:',NULL,multiple=T))
                                  ),
                                  mainPanel(
                                    column(8,tabPanel('Variables',h4(HTML("<b>Select Quasi-Identifier Variables:</b><br/>")),uiOutput('selectVars'),value='inputVars')
                                    ),
                                    column(4,tabPanel('Variable Types',h4(HTML("<b>Select Variable Types:</b><br/>")),uiOutput("variables"),value='inVarPanel')
                                    )
                                  )
                                  ,value='setupPanel'),
                         
                         tabPanel("Calculate",
                                  sidebarPanel(                              
                                    h4(HTML("<b>Download Key File:</b>")),
                                    downloadLink('downloadData', 'Download KEY.xlsx'),
                                    HTML("<br/>"),
                                    HTML("<br/>"),
                                    fluidRow(column(12,numericInput('probAttack','Probability of Attack HTML (only for advanced users, ask before changing)',0.3,min=0.025,max=1,step=0.025)),
                                             column(12,HTML("<br/>"),checkboxInput('includeFuzz',HTML('<b>Include Fuzz?</b>'),value=F)),
                                             column(12,h4(HTML("<b>Risk:</b>")),uiOutput("risk")),
                                             column(12,h4(HTML("<b>Loss:</b>")),uiOutput("loss")),
                                             column(12,h4(HTML('<b>Average Loss:</b>')),uiOutput('avg.loss')),
                                             column(12,h4(HTML('<b>Equivalence Classes:</b>')),uiOutput('eqvClassOut')))
                                  ),
                                  mainPanel(
                                    
                                    uiOutput("riskloss")
                                  )
                                  ,value='calcPanel'),
                         tabPanel("Explore",value='explorePanel',
                                  mainPanel(
                                    tabsetPanel(id='exploreSubPanel',
                                                tabPanel('Class Counts',tabsetPanel(id='countPanel')),
                                                tabPanel('Equivalence Classes',plotOutput('classPlot'),dataTableOutput("rinds")))
                                  )
                         )
                         
             )
             
  )
)

server <- function(input, output, session) {
  
  #Checks for list with only NULL values
  isNullList <- function(x) all(!lengths(x))
  
  ##############################
  ###   Display Variables
  ##############################
  
  #Read in dataset when file selected
  origds <- eventReactive(input$inDemogFile, {
    demogFile <- input$inDemogFile
    read_sas(demogFile$datapath)
  })
  
  #List variables with labels when file uploaded
  observeEvent(input$inDemogFile,{
    dnames <- names(origds())
    olabels <- origds() %>% map_chr(attr_getter("label"))
    dlabels <- paste(dnames,olabels,sep=" : ")
    
    output$selectVars <- renderUI(
      fluidRow(h5('(Do not select study and subject variables on this tab)'),
               column(12,checkboxGroupInput('selectedVars',label="Choose Variables:",choiceNames=dlabels,choiceValues=dnames,width='100%')))
    )
    getIDs()
  })
  
  getIDs <- function(){
    # getIDs <- eventReactive(input$inDemogFile,{
    print('study and subjid IDs')
    if (!'STUDYID' %in% names(origds())){
      showModal(modalDialog(
        title = "Study Number",
        'Variable STUDYID not found. Please Choose one or more variables to use as a Study Identifier',
        easyClose = TRUE,
        footer = NULL
      ))
      show(id='studyid')
      updateSelectInput(session,'studyid',choices=names(origds()))
    }
    if (!'USUBJID' %in% names(origds())){
      showModal(modalDialog(
        title = "Subject Number",
        'Variable USUBJID not found. Please Choose one or more variables to use as a Subject Identifier',
        easyClose = TRUE,
        footer = NULL
      ))
      show(id='usubjid')
      updateSelectInput(session,'usubjid',choices=names(origds()))
    }
  }
  
  #Give choices for type of variable for each chosen variable
  observeEvent(input$selectedVars,{
    print('selectedVars')
    output$variables <- renderUI({
      lapply(input$selectedVars,function(var){
        lastType <- isolate(input[[paste0('type',var)]])
        list(radioButtons(paste0('type',var),var,
                          choices=c("Numeric Grouping" = "num",
                                    "Categorization" = "cat",
                                    "Suppress" = "drop",
                                    "Retain" = "keep"),
                          selected=if(is.null(lastType)){character(0)}else{lastType}
        ))
      })
    })
  })
  
  allTypeTrigger <- reactive({
    alltmp <- list()
    for (var in input$selectedVars){
      tmp <- input[[paste0('type',var)]]
      alltmp <- append(alltmp,tmp)
    }
    return(alltmp)
  })
  
  riskIncludeVars <- reactive({
    tmpList <- list()
    for (var in paste0('rinclude',input$selectedVars)){
      tmpList[[var]] <- input[[var]]
    }
    return(tmpList)
  })
  
  eClassIncludeVars <- reactive({
    tmpList <- list()
    for (var in paste0('eclass',input$selectedVars)){
      tmpList[[var]] <- input[[var]]
    }
    return(tmpList)
  })
  
  #############################
  ##  Category Variables
  #############################
  
  varTrigger <- function(){
    req(!is.null(input$selectedVars))
    print('varTrigger')
    for (var in input$selectedVars){
      req(input[[paste0('type',var)]])
    }
  }
  
  varTypes <- reactive({
    varTrigger()
    print('varTypes')
    
    dynInputList <- list()
    for(dynInputs in paste0("type", input$selectedVars)){
      dynInputList[[dynInputs]] <- input[[dynInputs]]
    }
    return(dynInputList)
  })
  
  catTrigger <- eventReactive(varTypes(),{
    paste0('input[[paste0("catNum",',input$selectedVars,']]')
  })
  
  #Multiple trigger for number of categories
  catNums <- reactive({
    print('catNums')
    alltmp <- list()
    tmp <- list()
    for (var in input$selectedVars[varTypes() == 'num']){
      tmp <- input[[paste0('catNum',var)]]
      alltmp <- append(alltmp,tmp)
    }
    return(alltmp)
  })
  
  
  #Add dropdown for each requested category
  displayCharCats <- function(){
    print('catNums')
    inds <- origds()
    
    mapply(function(var,type){
      if (type == 'cat'){
        vchoice <- names(table(inds[,var]))
        names(vchoice) <- paste0(vchoice,' (n=',table(inds[,var]),')')
        observeEvent(input[[paste0('catNum',var)]],{
          output[[paste0('cat',var)]] <- renderUI(
            lapply(1:input[[paste0('catNum',var)]],function(fi){
              oldName <- isolate(input[[paste0('catName',fi,'_',var)]])
              oldValue <- isolate(input[[paste0('catValue',fi,'_',var)]])
              fluidRow(
                column(5,textInput(paste0('catName',fi,'_',var),'Name for Category:',value=oldName)),
                column(7,selectInput(paste0('catValue',fi,'_',var),"Choose:",vchoice,multiple=T,selected=oldValue))
              )
            })
          )
        })
      }
    },    
    input$selectedVars,
    varTypes())
  }
  
  charCats <- reactive({
    print('charCats')
    alltmp <- list()
    for (var in input$selectedVars[varTypes() == 'cat']){
      cnum <- input[[paste0('catNum',var)]]
      req(cnum)
      for (catNum in 1:cnum){
        listValues <- input[[paste0('catValue',catNum,'_',var)]]
        tmp <- list(name=list(input[[paste0('catName',catNum,'_',var)]]),values=listValues)
        if (length(tmp) == 1){
          tmp <- c('','')
        }
      }
      alltmp[[var]] <- tmp
    }
    return(alltmp)
  })
  
  ###########################
  ##   Numeric Variables
  ###########################
  
  numTrigger <- reactive({
    req(input$mainTabSet != 'setupPanel')
    print('numTrigger')
    alltmp <- list()
    nvars <- input$selectedVars[varTypes() == 'num']
    tmp <- list()
    for (var in nvars){
      req(input[[paste0('lowerLimit',var)]])
      tmp <- c(input[[paste0('lowerLimit',var)]],input[[paste0('range',var)]],input[[paste0('upperLimit',var)]])
      alltmp[[var]] <- tmp
    }
    names(alltmp) <- nvars
    return(alltmp)
  })
  
  
  numLimits <- eventReactive(numTrigger(),{
    print('numList')
    numVars <- input$selectedVars[varTypes() == 'num']
    req(input$mainTabSet == 'calcPanel')
    tmp <- lapply(numVars,function(var){
      list(input[[paste0('lowerLimit',var)]],input[[paste0('range',var)]],input[[paste0('upperLimit',var)]])
    })
    names(tmp) <- numVars
    return(tmp)
  },ignoreNULL=T)
  
  
  ################################
  ##  Risk and Loss
  ################################
  
  #Give detail choices by variable type when type choices are made
  observeEvent(input$mainTabSet != 'setupPanel',{
    print('riskloss')
    #Get input data
    inds <- origds()
    
    #Give choices for each variable by type
    output$riskloss <- renderUI({ list(
      lapply(input$selectedVars,function(var){
        type <- varTypes()[paste0('type',var)]
        oldFuzz <- isolate(input[[paste0('fuzz',var)]])
        oldInclude <- isolate(input[[paste0('rinclude',var)]])
        oldClass <- isolate(input[[paste0('eclass',var)]])
        if (type == 'num'){
          lower <- NULL
          lower <- isolate(input[[paste0('lowerLimit',var)]])
          range <- isolate(input[[paste0('range',var)]])
          upper <- isolate(input[[paste0('upperLimit',var)]])
          tryCatch({
            stepds <- diff(sort(unlist(inds[,var])))
            step <- signif(min(stepds[stepds != 0],na.rm=T),1)
          },
          error=function(cond){
            showModal(modalDialog(
              title = "Error",
              paste0('Variable type error. Variable ',var,' set as numeric variable but error occurred. Variable will be set to Categorization'),
              easyClose = TRUE,
              footer = NULL
            ))
            updateRadioButtons(session,paste0('type',var),selected=character(0))
          })
          
          if (!is.null(lower)){
            list(
              tryCatch({
                fluidRow(
                  titlePanel(h4(HTML(paste('<b>',paste('&nbsp',var),'</b><br/>')))),
                  column(2,numericInput(paste0('lowerLimit',var),'Lower',min=signif(min(inds[,var],na.rm=T),1),max=signif(max(inds[,var],na.rm=T),1),value=if(is.null(lower)){min(inds[,var],na.rm=T)}else{lower})),
                  column(3,sliderInput(paste0('range',var),var,min=0,max=signif(max(inds[,var],na.rm=T)-min(inds[,var],na.rm=T),1),value=if(is.null(range)){0}else{range},step=step)),
                  column(2,numericInput(paste0('upperLimit',var),'Upper',min=signif(min(inds[,var],na.rm=T),1),max=signif(max(inds[,var],na.rm=T),1),value=if(is.null(upper)){max(inds[,var],na.rm=T)+1}else{upper})),
                  column(1,numericInput(paste0('fuzz',var),'Fuzz:',value=if(is.null(oldFuzz)){0}else{oldFuzz})),
                  column(2,checkboxInput(paste0('rinclude',var),'Include in Risk Calculation?',value=if(is.null(oldInclude)){T}else{oldInclude})),
                  column(2,checkboxInput(paste0('eclass',var),'Include in Equivalence Classes',value=if(is.null(oldClass)){F}else(oldClass)))
                )
              },
              error=function(cond){
                showModal(modalDialog(
                  title = "Error",
                  paste0('Variable type error. Variable ',var,' set as numeric variable but error occurred. Variable will be set to Categorization'),
                  easyClose = TRUE,
                  footer = NULL
                ))
                updateRadioButtons(session,paste0('type',var),selected='cat')
                updateTabsetPanel(session,selected='setupPanel')
              })
            )
          } else {
            fluidRow(
              titlePanel(h4(HTML(paste('<b>',paste('&nbsp',var),'</b><br/>')))),
              column(2,numericInput(paste0('lowerLimit',var),'Lower',min=signif(min(inds[,var],na.rm=T),1),max=signif(max(inds[,var],na.rm=T),1),value=min(inds[,var],na.rm=T))),
              column(3,sliderInput(paste0('range',var),var,min=0,max=signif(max(inds[,var],na.rm=T)-min(inds[,var],na.rm=T),1),value=0,step=step)),
              column(2,numericInput(paste0('upperLimit',var),'Upper',min=signif(min(inds[,var],na.rm=T),1),max=signif(max(inds[,var],na.rm=T),1),value=max(inds[,var],na.rm=T))),
              column(1,numericInput(paste0('fuzz',var),'Fuzz:',value=0)),
              column(2,checkboxInput(paste0('rinclude',var),'Include in Risk Calculation?',value=T)),
              column(2,checkboxInput(paste0('eclass',var),'Include in Equivalence Classes',value=F)))
          }
          
        } else if (type == 'cat'){
          print('cat fault')
          catNum <- isolate(input[[paste0('catNum',var)]])
          # oldInclude <- isolate(input[[paste0('rinclude',var)]])
          fluidRow(
            titlePanel(h4(HTML(paste('<b>',paste('&nbsp',var),'</b><br/>')))),
            column(12,numericInput(paste0('catNum',var),'Number of New Categories:',if(is.null(catNum)){1}else{catNum},min=1)),
            column(8,tabPanel('Categories:',uiOutput(paste0('cat',var)))),
            column(2,checkboxInput(paste0('rinclude',var),'Include in Risk Calculation?',value=if(is.null(oldInclude)){T}else{oldInclude})),
            column(2,checkboxInput(paste0('eclass',var),'Include in Equivalence Classes',value=if(is.null(oldClass)){F}else(oldClass)))
          )
        } else {
          fluidRow(
            titlePanel(h4(HTML(paste('<b>',paste('&nbsp',var),'</b><br/>')))),
            column(4,checkboxInput(paste0('rinclude',var),'Include in Risk Calculation?',value=if(is.null(oldInclude)){T}else{oldInclude})),
            column(4,checkboxInput(paste0('eclass',var),'Include in Equivalence Classes',value=if(is.null(oldClass)){F}else(oldClass)))
          )
        } 
      })
    )})
    displayCharCats()
  },ignoreNULL=T,ignoreInit=T)
  
  #################################
  ##  Create Data Categories
  #################################
  
  #Create DI variables
  apds <- function(fuzz=F,initVars=F){
    print('apds')
    ns <- origds()
    vars <- input$selectedVars
    nums <- numTrigger()
    types <- varTypes()
    
    #Loop through each input variable
    for (var in vars){
      divar <- paste0(var,'DI')
      if (initVars){
        ns[,divar] <- ns[,var]
      } else {
        if (types[paste0('type',var)] == 'num'){
          lower <- nums[[var]][1]
          range <- nums[[var]][2]
          upper <- nums[[var]][3]
          
          #If less than lower bound
          ns[,divar] <- apply(ns,1,function(x){
            ifelse(as.numeric(x[var]) < lower,paste0('<',lower),
                   #If greater than upper bound
                   ifelse(as.numeric(x[var]) >= upper,paste0('>=',upper),
                          if (range > 0){
                            paste0(floor((as.numeric(x[var])-lower)/range)*range+lower,'-<',min(floor((as.numeric(x[var])-lower)/range)*range+range+lower,upper))
                          } else {
                            as.numeric(x[var])
                          }
                   )
            )})
          # }
        } else if (types[paste0('type',var)] == 'cat'){
          print('inCat')
          numGrps <- input[[paste0('catNum',var)]]
          
          if (!isNullList(charCats())){
            for (numGrp in 1:numGrps){
              if (!isNullList(charCats())){
                catVar <- charCats()[[var]]
                catName <- catVar[['name']]
                catValues <- catVar[['values']]
                if (!isNullList(catValues)){
                  if (paste0(var,'DI') %in% colnames(ns)){
                    ns[,divar] <- unlist(apply(ns,1,function(x){
                      ifelse(x[var] %in% catValues,catName,x[paste0(var,'DI')])
                    }))
                  } else {
                    ns[,divar] <- unlist(apply(ns,1,function(x){
                      ifelse(x[var] %in% catValues,catName,x[var])
                    }))
                  }
                } else {
                  ns[,divar] <- ns[,var]
                }
                
              }
            }
          } else {
            ns[,divar] <- ns[,var]
          }
        } else {
          ns[,divar] <- ns[,var]
        }
      }
    }
    print('apds done')
    return(ns)
  }
  
  ####################################
  ##  Loss
  ####################################
  
  loss.byvar <- function(df,vars){
    print('loss.byvar')
    #Get counts for all unique values by variable
    cnt <- lapply(df[,vars],function(x){
      table(x)
    })
    #Get total number of subjects
    tot.cnt <- lapply(cnt,sum)
    entropy <- list()
    #Calculate entropy for each record and variable
    for (var in vars){
      entropy[var] <- lapply(cnt[var],function(x){
        x/as.numeric(tot.cnt[var])*log2(x/as.numeric(tot.cnt[var]))
      })
    }
    #Get total entropy by variable
    entropy.byvar <- lapply(entropy,sum)
    return(entropy.byvar)
  }
  
  updateLoss <- function(inds){
    print('loss')
    
    lossvars <- names(riskIncludeVars())[unlist(riskIncludeVars())]
    lossvars <- substr(lossvars,9,nchar(lossvars))
    
    #Calculate original entropy by variable
    ods <- origds()
    bentropy <- loss.byvar(ods,lossvars)
    
    #Calculate entropy for anonymized variables
    aentropy <- loss.byvar(inds,paste0(lossvars,'DI'))
    
    #Calculate percent change in entropy by variable
    loss <- mapply(function(before,after,var){
      paste0(var,' = ',round(((before-after)/before)*100,digits=2),'%')
    },bentropy,aentropy,paste0(lossvars,'DI'))
    
    #Display loss by variable with one variable per row
    output$loss <- renderText({
      HTML(paste(as.character(loss),collapse="<br/>"))
    })
    rLoss$data <- loss
    
    #Average Loss
    losses <- mapply(function(before,after,var){
      round(((before-after)/before)*100,digits=2)
    },bentropy,aentropy,paste0(lossvars,'DI'))
    output$avg.loss <- renderUI(paste(round(mean(unlist(losses),na.rm=T),digits=2),'%'))
    
    for (var in lossvars){
      aentropy[var] <- aentropy[paste0(var,'DI')]
      aentropy <- aentropy[!names(aentropy) == paste0(var,'DI')]
    }
    
    tmp <- t(mapply(function(before,after,var){
      c(round(((before-after)/before)*100,digits=2))
    },unlist(bentropy),unlist(aentropy),lossvars))
    avgloss <- mean(unlist(tmp),na.rm=T)
    print(avgloss)
    rAvgLoss$data <- avgloss
  }
  
  ######################################
  ## Risk
  #####################################
  
  getNewds <- function(){
    print('getNewds')
    ds <- apds()
    return(ds)
  }
  
  rinds <- function(rds1,includeVars,class=F){
    print('rinds')
    # rvars <- names(riskIncludeVars())[unlist(riskIncludeVars())]
    rvars <- names(includeVars)[unlist(includeVars)]
    if (class){
      rvars <- substr(rvars,7,nchar(rvars))
      print('class')
      print(rvars)
    } else {
      rvars <- substr(rvars,9,nchar(rvars))
      print('class')
      print(rvars)
    }
    # rvars <- substr(rvars,9,nchar(rvars))
    kvars <- rvars[!rvars %in% input$selectedVars[varTypes() == 'drop']]
    kvars <- paste0(kvars,'DI')
    rds1[,kvars] %>%
      group_by(!!!syms(kvars)) %>%
      summarize(n=n()) %>%
      arrange(n)
  }
  
  rinds2 <- function(rds1){
    rds1 %>%
      group_by(!!!syms(colnames(rds1))) %>%
      summarize(n=n()) %>%
      arrange(n)
  }
  
  getRiskData <- function(subset='N',initVars=F,class=F){
    print('getRiskData')
    # riskds <- apds(fuzz=input$includeFuzz,initVars=initVars)
    # rvars <- names(riskIncludeVars())[unlist(riskIncludeVars())]
    # rvars <- substr(rvars,9,nchar(rvars))
    if (class){
      riskds <- apds(initVars=initVars)
      rvars <- names(eClassIncludeVars())[unlist(eClassIncludeVars())]
      rvars <- substr(rvars,7,nchar(rvars))
      print('class riskdata')
      print(rvars)
    } else {
      riskds <- apds(fuzz=input$includeFuzz,initVars=initVars)
      rvars <- names(riskIncludeVars())[unlist(riskIncludeVars())]
      rvars <- substr(rvars,9,nchar(rvars))
      print('class riskdata')
      print(rvars)
    }
    kvars <- rvars[!rvars %in% input$selectedVars[varTypes() == 'drop']]
    if (subset == 'Y'){
      print('getrisk done')
      riskds[,paste0(kvars,'DI')]
    } else {
      print('getrisk done')
      riskds
    }
  }
  
  rTotRisk <- reactiveValues(data={0})
  rLoss <- reactiveValues(data={0})
  rAvgLoss <- reactiveValues(data={0})
  
  dataInit <- reactiveVal({F})
  updateRisk <- function(){
    print('risk')
    #Function for risk of re-identification
    risk <- function(ds){
      vars <- colnames(ds)
      grps <- unique(ds[,vars])
      
      #Correction for missing values
      allMiss <- data.frame()
      for (i in 1:length(vars)){
        missrows <- grps %>% filter(!!sym(vars[i]) %in% c('NOT REPORTED','',' '))
        chkvar <- vars[i]
        mgvars <- vars[-i]
        for (j in 1:nrow(missrows)){
          missmatch <- grps %>% inner_join(missrows[j,-i],by=mgvars)
          if (nrow(missmatch) > 0){
            allMiss <- rbind(allMiss,missrows[j,])
          }
        }
      }
      
      #Calculate Risk
      ngrps <- nrow(grps) - nrow(allMiss)/2
      pop <- nrow(ds)
      risk <- round((ngrps/pop)*input$probAttack,digits=4)
    }
    
    #Calculate risk for all DI variables
    if (dataInit()){
      subRisk <- getRiskData(subset='Y')
      updateLoss(getRiskData())
    } else {
      subRisk <- getRiskData(subset='Y',initVars=T)
      updateLoss(getRiskData(initVars=T))
      dataInit(T)
    }
    
    tot.risk <- risk(subRisk)
    rTotRisk$data <- tot.risk
    print(tot.risk)
    
    #Display risk as red if over limit
    output$risk <- renderUI({
      if (tot.risk > 0.09){
        HTML(paste("<span style=color:#ff5733><b>",h2(tot.risk),"</b></span>"))
      } else {
        tot.risk
      }
    })
    
    if (sum(unlist(eClassIncludeVars())) > 0){
      print('eq Classes')
      print(eClassIncludeVars())
      print(getRiskData(subset='Y'))
      eqds <- rinds(getRiskData(subset='Y',class=T),eClassIncludeVars(),class=T)
      print('eqds done')      
      #Display percentage of equivalence class counts
      numClasses <- sum(eqds$n)
      classGrps <- eqds %>% mutate(classGrp=ifelse(n>9,'At least 10',ifelse(n>=3,'At least 3',ifelse(n==1,'Single Class','not')))) %>%
        group_by(classGrp) %>% summarize(nsum=sum(n)) %>%
        mutate(perc=round((nsum/numClasses)*100,1))
      
      eqvClassOut <- c(paste0('Single Member:',max(classGrps[classGrps$classGrp == 'Single Class','perc'],0,na.rm=T),'%'),paste0('At least 3 Members:',max(classGrps[classGrps$classGrp == 'At least 3','perc'],0,na.rm=T),'%'),
                       paste0('At least 10 Members:',max(classGrps[classGrps$classGrp == 'At least 10','perc'],0,na.rm=T),'%'))
      
      output$eqvClassOut <- renderText({
        HTML(paste(as.character(eqvClassOut),collapse="<br/>"))
      })
    }
  }
  
  observeEvent(input$includeFuzz,{
    updateRisk()
  })
  
  observeEvent(riskIncludeVars(),{
    updateRisk()
  })
  
  observeEvent(list({input$mainTabSet == 'explorePanel'},input$exploreType),{
    exploreData()
  },ignoreNULL=T,ignoreInit=T)
  
  #Download csv file of key with DI variables matched to USUBJID
  output$downloadData <- downloadHandler(
    filename = "key.xlsx",
    content = function(file) {
      print('download')
      rds <- getNewds()
      
      studyvar <- "STUDYID"
      subjvar <- "USUBJID"
      if (!isNullList(input$usubjid)){
        studyvar <- input$studyid
      }
      if (!isNullList(input$studyid)){
        subjvar <- input$usubjid
      }
      dskey <- rds[,c(studyvar,subjvar,paste0(input$selectedVars[!varTypes() %in% c('drop','keep')],'DI'))]
      orig <- origds()
      divars <- paste0(input$selectedVars,'DI')
      
      for (divar in input$selectedVars){
        res <- Map(`%in%`, orig[,divar],rds[,paste0(divar,'DI')])
        res[lengths(res) == 0] <- FALSE
        if (sum(unlist(res)) == nrow(orig)){
          dskey <- dskey[,!names(dskey) %in% c(paste0(divar,'DI'),paste0(divar[input[[paste0('type',divar)]] %in% c('drop','keep')]))]
        }
      }
      
      evars <- colnames(dskey)[!colnames(dskey) %in% c(studyvar,subjvar)]
      eclass <- rinds2(rds[,evars])
      
      newNames <- mapply(function(name,ki){
        if (varTypes()[ki] == 'keep'){
          paste0(substr(name,1,nchar(name)-2),'__RE')
        } else if (varTypes()[ki] == 'drop'){
          paste0(substr(name,1,nchar(name)-2),'__SU')
        } else if (varTypes()[ki] == 'num'){
          paste0(substr(name,1,nchar(name)-2),'__NG')
        } else if (varTypes()[ki] == 'cat'){
          paste0(substr(name,1,nchar(name)-2),'__CA')
        }
      },names(dskey[,c(-1,-2)]),seq_along(names(dskey[,c(-1,-2)])))
      colnames(dskey) <- c(studyvar,subjvar,newNames)
      names(eclass) <- c(newNames,'Number of Subjects')
      
      if ('num' %in% varTypes()){
        allNum <- data.frame()
        allNumf <- function(name,type,limit){
          for (i in 1:length(name)){
            if (type[i] == 'num'){
              tmp <- data.frame(variable=name[i],lowerLimit=unlist(limit[name[i]][[1]][1]),interval=unlist(limit[name[i]][[1]][2]),upperLimit=unlist(limit[name[i]][[1]][3]),includeInRisk=unlist(input[[paste0('rinclude',name[i])]]))
              allNum <- rbind(allNum,tmp)
            }
          }
          allNum
        }
        allNum <- allNumf(input$selectedVars[varTypes() == 'num'],varTypes(),numLimits())
        
      } else {
        allNum <- list()
      }
      print('allNum')
      
      chars <- data.frame()
      allCharf <- function(var,type,choice){
        for (ci in 1:length(var)){
          if (type[ci] == 'cat'){
            for (gi in 1:input[[paste0('catNum',var[ci])]]){
              values <- input[[paste0('catValue',gi,'_',var[ci])]]
              charsTmp <- data.frame(variableName=rep(var[ci],length(values)),categoryName=rep(input[[paste0('catName',gi,'_',var[ci])]],length(values)),categoryValue=values,includeInRisk=unlist(input[[paste0('rinclude',var[ci])]]))
              chars <- rbind(chars,charsTmp)
            }
          }
        }
        return(chars)
      }
      
      allChar <- allCharf( input$selectedVars,varTypes(),charCats())
      print('allChar')
      
      dskeys <- list()
      dskeys <- lapply(paste0(input$selectedVars[!varTypes() %in% c('drop','keep')],'DI'),function(var){
        rds %>% group_by(!!sym(var)) %>% summarize(n=n())
      })
      
      names(dskeys) <- input$selectedVars[!varTypes() %in% c('drop','keep')]
      dskeys[['Key']] <- dskey
      dskeys[['NumericMapping']] <- allNum
      dskeys[['CategoryMapping']] <- allChar
      
      tmp <- c('Percent Average Loss',round(rAvgLoss$data,digits=2))
      tmp2 <- c('Total Risk Score',rTotRisk$data)
      tmp3 <- t(rLoss$data)
      tmp4 <- rbind(tmp,tmp2)
      
      dskeys[['Risk']] <- tmp4
      dskeys[['Classes']] <- eclass
      
      dsnames <- names(dskeys)
      newnames <- c(tail(dsnames,5),head(dsnames,length(dsnames)-5))
      dskeys <- dskeys[newnames]
      write.xlsx(dskeys, file,rowNames=F)
    }
  )
  
  observeEvent(list(charCats(),numLimits()),{
    print('HERE')
    updateRisk()
  },ignoreInit=T)
  
  fuzzChg <- reactive({
    paste0('input$fuzz',input$selectedVars)
  })
  
  ######################################
  ## Explore and Download
  ######################################
  
  exploreData <- function(){
    print('exploreData')
    exploreData <- getRiskData()
    eplotds <- rinds(exploreData,riskIncludeVars())
    print(eplotds)
    output$rinds <- renderDT(eplotds)
    print('eplot')    
    #Display percentage of equivalence class counts
    numClasses <- sum(eplotds$n)
    eplotds2 <- eplotds %>% mutate(classLab=ifelse(n >= 9,'9 or more',as.character(n))) %>%
      group_by(classLab) %>% summarize(nsum=sum(n)) %>%
      mutate(perc=round((nsum/numClasses)*100,1))
    print('eplot2')    
    output$classPlot <- renderPlot({
      ggplot(eplotds2,aes(x=classLab,y=perc,fill=as.factor(perc))) +
        geom_bar(stat='identity') +
        geom_text(aes(label=paste0(perc,'%')),vjust=-1) +
        theme(legend.position="none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 12)) +
        labs(x='Equivalence Class Size',y='Percentage of Total Classes')
    })
    print('mapply') 
    mapply(function(var,varname,origvar,type){
      removeTab('countPanel',substr(var,1,nchar(var)-2))
      if (type == 'num'){
        origvar <- substr(var,1,nchar(var)-2)
        plotds <- exploreData %>% group_by(!!sym(var)) %>% summarize(n=n(),maxval=max(!!sym(origvar))) %>% arrange(maxval) %>% mutate(plotval=!!sym(var))
      }
      else {
        plotds <- exploreData %>% group_by(!!sym(var)) %>% summarize(n=n()) %>% arrange(n) %>% mutate(plotval=!!sym(var))
      }
      
      insertTab('countPanel',tabPanel(substr(var,1,nchar(var)-2),plotOutput(paste0(varname,'plot'),height='600px'),fluidRow(dataTableOutput(varname))))
      
      coul <- rainbow(nrow(plotds))
      output[[paste0(varname,'plot')]] <- renderPlot({par(mar = c(20,5,2,2))
        barplot(height=plotds$n,names=plotds$plotval,las=2,col=coul)})
      output[[varname]] <- renderDT(datatable(plotds[,c(var,'n')]) %>% formatStyle(
        var,
        color = styleEqual(
          as.vector(unlist(plotds[,var])), rainbow(nrow(plotds))
        ),
        fontWeight = 'bold'
      ))
      updateTabsetPanel(session,'exploreSubPanel',selected='countPanel')
    },paste0(input$selectedVars,'DI'),
    paste0('varcnt',input$selectedVars),
    input$selectedVars,
    varTypes()
    )
  }
}

shinyApp(ui, server)
