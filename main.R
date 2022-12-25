# Linear Regression


#########################################################
# Loading all common libraries
#########################################################
library(shiny)
library(shinydashboard)    # for Dashboard
library(shinyWidgets)      # for radio button widgets
library(shinyalert)        # for alert message very nice format
library(dplyr)             # select and pipe %>% functions are covered in the library
library(DT)                # for DT data table and formatting
library(shinyjs)           # to perform common useful JavaScript operations in Shiny apps
library(ggplot2)           # to draw or take advantage of ggplot functions
library(ggthemes)          # to take the benefit of gg themes
library(shinyBS)           # to add bsTooltip 

#########################################################
# Loading libraries specific to Linear Regression
#########################################################
library(olsrr)             #Excellent package for Linear Regression all plots and tests
library(nortest)           #for normality test - #Anderson-Darling normality test
library(tseries)           # for Jarque-Bera test of normality of residuals in R
library(lmtest)            # for statistical test for Homoscedasticity
library(flashCard)         # to flip the box like flipbox in shinydashboardPlus package
# library(kableExtra)        # to handle kbl tables and scroll_box function
library(ggfortify)         # to plot all diagnostics for lm model together
library(gridExtra)         # grid view of multiple plots
library(stringr)           # to wrap lengthy text in plot title
library(car)               # to do durbinWatsonTest to check residuals autocorrelation

#################################################################################
#style function for Action button default 50px and width 180px; you can change
#################################################################################
styleButtonBlue<- function(xheight="50px",xwidth="auto",xcolor='#4682B4'){
  paste("white-space: normal;
  text-align:center;
  color: #ffffff;
        background-color:",xcolor,";",
        "border-color: #ffffff;
        border-width:2px;
        height:",xheight,";
        width:",xwidth,";
        font-size: 13px;")
}

##################################################################
# define 'not in' func
##################################################################
'%!in%' <- Negate('%in%')  # define 'not in' func


##################################################################
#URL links are defined here and used on the dashboard
##################################################################

urlvif <- a(HTML(paste('<h5><b>',"Reference: Statistics How To, 'Statistics for the rest of us!'",'</b><br><h5>')),
            href="https://www.statisticshowto.com/variance-inflation-factor/")


urlRegn1  <- a("Linear Regression in Machine Learning on Java T Point ", href="https://www.javatpoint.com/linear-regression-in-machine-learning")
urlRegn2  <- a("Assumptions of Linear Regression by Pavan Vadapalli on www.upgrad.com", href="https://www.upgrad.com/blog/assumptions-of-linear-regression/")
urlRegn3  <- a("Complete Introduction to Linear Regression in R by  Selva Prabhakaran", href="https://www.machinelearningplus.com/machine-learning/complete-introduction-linear-regression-r/")
urlRegn4  <- a("Evaluation of Linear Regression Models by Jagandeep Singh", href="https://medium.com/datadriveninvestor/evaluation-of-linear-regression-models-257851e77e28")
urlRegn5  <- a("How to Perform a Durbin-Watson Test in R at statology.org", href="https://www.statology.org/durbin-watson-test-r/",href="https://www.statology.org/scale-location-plot/")

urlRegn8  <- a("How To Run A Normality Test in R (Statistical tests) at programmingr.com", href="https://www.programmingr.com/statistics/normality-test/")
urlRegn9  <- a("Plotting Diagnostics for Linear Models with 'ggfortify' package at cran.r-project.org ", href="https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_lm.html")
urlRegn10 <- a("Understanding Diagnostic Plots for Linear Regression Analysis by Bommae Kim, Statistical Consulting Associate",href="https://data.library.virginia.edu/diagnostic-plots/")

#https://www.statology.org/scale-location-plot/



##################################################################
#Dashboard header starts here
##################################################################
header<- dashboardHeader(
  titleWidth = '475px',
  tags$li(
    tags$head(tags$style(HTML('.navbar-custom-menu {width:500; float: left!important;}'))),  ## where you got: https://stackoverflow.com/questions/60861004/move-logo-to-the-left-side-of-dashboard-header-in-shiny-dashboard
    actionButton(inputId = "Previous", label = icon("arrow-left")),
    actionButton(inputId = "Next", label = icon("arrow-right")),
    class = "dropdown"
  )
)


##################################################################
#Dashboard sidebar starts here
##################################################################
sidebar <- dashboardSidebar(
  # Remove the sidebar toggle element
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  minified = FALSE,  #this option will completely close the side bar  and expand the header text
  id = 'msidebarid',
  collapsed = TRUE,
  useShinyjs(),
  
  sidebarMenu(id = "tabs",
              menuItem('Data Processing',
                       tabName = 'tab_Dataprocessing'),
              menuItem('Normal Distribution Test',
                       tabName = 'tab_TestAssumption')
  )#sidebar menu
)


##################################################################
#Dashboard Body starts here
##################################################################
body <- dashboardBody(
  useShinyalert(force = TRUE),
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  shinyjs::useShinyjs(),
  tabItems(
    
    tabItem(
      tabName = "tab_Dataprocessing",
      column(
        width = 12,
        align ='center',
        box(
          id = "mprocBox0",
          width = 2,
          height = '500px',
          align = "center",
          title = "Data Process Menu",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          br(), br(),
          actionButton(inputId = 'mDataUploadBtn',label = "Data Upload!",style = styleButtonBlue(xheight = '35px',xwidth = '162px')),
          br(),
          actionButton(inputId = 'mshowtableBtn',label = "Review Dataset!",style = styleButtonBlue(xheight = '35px',xwidth = '162px')),
          br(),
          actionButton(inputId = 'mVisualizationBtn',label = "Visualization Dataset!",style = styleButtonBlue(xheight = '35px',xwidth = '162px')),
          br(),
          actionButton(inputId = 'mFixDependentVarBtn',label = "Choose Dependent Variable!",style = styleButtonBlue(xheight = '70px',xwidth = '162px'))
        ),#box closure
        column(
          width = 10,
          tags$div(id = 'placeholder_MultiPurpose')
        )
      )#column closure
      #) # box closure
    ),#tabitem closure
    
    tabItem(
      tabName = "tab_TestAssumption",
      tabsetPanel(id = 'tabsetPanelAssumption',
                  tabPanel(
                    "Linearity",
                    column(
                      width = 12,
                      offset = 0,
                      align = "left",
                      fluidRow(
                        tags$head(
                          tags$style(
                            options(width=1500),
                            paste0("#mLinearityPlot{color:black; font-size:14px; font-style:bold;overflow:scroll;text-align: center;
                                            width: 100%;height: 400px;max-height: 400px; background: #ffffff;}")
                          )
                        ),
                        
                        
                        box(
                          HTML(paste('<h5><b><CENTER>',"Test of Linearity between Dependent and Independent Variables",'</CENTER></b>')),
                          HTML(paste('<h5><b><CENTER>',actionLink(inputId = 'mLinearityLink',label = 'Click Me!'),'</CENTER></b>')),
                          width = 12,
                          height = '500px',
                          title = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = FALSE,
                          plotOutput('mLinearityPlot',width = '100%',height = '400px'),
                          bsTooltip(
                            id = "mLinearityLink",
                            title = HTML(paste('<h5><b>',"Click Me to learn more",'</b><h6>',"The first assumption is that a linear relationship exists between the independent variable, x, and the dependent variable, y.",'<br>',
                                               "Visually from the scatter plot, you can check whether the points follow the stright line;",
                                               'if so there exists a linear relationship')),
                            trigger = "hover",
                            placement = "right", 
                            options = list(container = "body"))
                        )#box closure
                      ) #fluid Row Closure
                    )#column closure
                  ),#Tabpanel closure
                  tabPanel(
                    "No Multicollinearity",
                    column(
                      width = 12,
                      offset = 0,
                      align = "left",
                      fluidRow(
                        box(
                          HTML(paste('<h5><b><CENTER>',"Test of Multicollinearity among Predictors / Independent Variables",'</CENTER></b>')),
                          HTML(paste('<h5><b><CENTER>',actionLink(inputId = 'mMulticollinearityLink',label = 'Click Me!'),'</CENTER></b>')),
                          width = 12,
                          height = '500px',
                          title = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = FALSE,
                          HTML(paste('<h5><b>',"Intro",'</b><br>',
                                     "Multicollinearity exists when independent variables in regression model are highly correlated to each other.",
                                     "For example, salary and experience are closely related / correlated; any model considering these variables as predictors will face the problem of multicollinearity",
                                     "You might not be able to trust the p-values to identify independent variables that are statistically significant.",
                                     "A variance inflation factor(VIF) detects multicollinearity in regression analysis. The R function of vif() is used and the VIF scores of predictors for our dataset are reported here: ",'<br>','<br>')),
                          verbatimTextOutput("predictorMulticollinearity"),
                          br(),
                          column(
                            width = 12,
                            uiOutput("predictorMulticollinearityTxt") 
                          )
                        )#box closure
                      ) #fluid Row Closure
                    )#column closure
                  ),#Tabpanel closure
                  tabPanel(
                    "Independence",
                    column(
                      width = 12,
                      offset = 0,
                      align = "center",
                      fluidRow(
                        tags$head(
                          tags$style(
                            paste0("#mIndependenceTxt1{color:black; font-size:10px; font-style:bold;overflow:auto;text-align: justify;margin: 5px 5px 5px 5px;
                                            width: '100%';height: 375px;max-height: 375px; background: #ffffff;}"),
                            paste0("#mIndependenceTxt2{color:black; font-size:10px; font-style:bold;overflow:auto;text-align: left;margin: 5px 5px 5px 5px;
                                            width: '100%';height: 375px;max-height: 375px; background: #ffffff;}"),
                            
                            "#uiOutput{color:black; font-size:8.5px; font-style:normal;
                                            max-height: 400px; background: light-blue;text-align: left;}"
                          )
                        ),
                        box(
                          width = 12,
                          HTML(paste('<h5><b><CENTER>',"Test-Residuals are Independent",'</CENTER></b>')),
                          HTML(paste('<h6><b><CENTER>',"Reference:",urlRegn5,'</CENTER></b><h6>')),
                          height = '500px',
                          title = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = FALSE,
                          
                          column(
                            width = 6,
                            height = '375px',
                            align='left',
                            uiOutput("mIndependenceTxt1")
                          ),
                          column(
                            width = 6,
                            height = '375px',
                            align='left',
                            uiOutput("mIndependenceTxt2")
                          )
                        ) #box closure
                      ) #fluid Row Closure
                    )#column closure
                  ),#Tabpanel closure
                  tabPanel(
                    "Homoscedasticity",
                    column(
                      width = 12,
                      offset = 0,
                      fluidRow(
                        box(
                          HTML(paste('<h5><b><CENTER>',"Test-Residuals have constant variance",'</CENTER></b>')),
                          HTML(paste('<h5><b><CENTER>',actionLink(inputId = 'mHomoLink',label = 'Click Me!'),'</CENTER></b>')),
                          width = 12,
                          height = '500px',
                          title = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = FALSE,
                          plotOutput('mHomoscedasticityPlot', height = '425px'),
                          bsTooltip(
                            id = "mHomoLink",
                            title = HTML(paste('<h5><b>',"Click Me to learn more",'</b><h6>',"The next assumption of linear regression is that the residuals have constant variance at every level of x.",'<br>',
                                               "The scatter plot shows the fitted-values of the model Vs residuals of those fitted values;",'<br>',
                                               "It is used to detect non-linearity and unequal error variances. If the red line across the center of the plot is roughly horizontal then we can assume that the residuals follow a linear pattern.")),
                            trigger = "hover",
                            placement = "right", 
                            options = list(container = "body"))
                        )#box closure
                      ) #fluid Row Closure
                    )#column closure
                  ),#Tabpanel closure
                  
                  tabPanel(
                    "Normality",
                    column(
                      width = 12,
                      offset = 0,
                      fluidRow(
                        box(
                          HTML(paste('<h5><b><CENTER>',"Test-Residuals follow Normally Distirbution using QQ Plot",'</CENTER></b>')),
                          HTML(paste('<h5><b><CENTER>',actionLink(inputId = 'mQQLink',label = 'Click Me!'),'</CENTER></b>')),
                          width = 12,
                          height = '500px',
                          title = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = FALSE,
                          plotOutput('mQQNormalityPlot', height = '425px'),
                          bsTooltip(
                            id = "mQQLink",
                            title = HTML(paste('<h5><b>',"Click Me to learn more",'</b><h6>',
                                               "The next assumption is that the residuals generated by the model follow a normal distribution.",'<br>',
                                               "Visually from the QQ Plot, the data points must fall along a rough straight line of 45 degree angles, for the residuals to be normally distributed.")),
                            trigger = "hover",
                            placement = "right", 
                            options = list(container = "body"))
                        )#box closure
                      ) #fluid Row Closure
                    )#column closure
                  ),#Tabpanel closure
                  tabPanel(
                    "Measures of Influence",
                    column(
                      width = 12,
                      offset = 0,
                      fluidRow(
                        box(
                          uiOutput("mMItabtitle"),
                          HTML(paste('<h5><b><CENTER>',
                                     actionLink(inputId = 'mMILink',label = 'Click Me!'),'&nbsp;','&nbsp;','&nbsp;',
                                     actionLink(inputId = 'mChangeChart',label = 'Change Chart Type!'),'</CENTER></b>')),
                          width = 12,
                          height = '500px',
                          title = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = FALSE,
                          plotOutput('mMeasureofIncluencePlot', height = '425px'),
                          bsTooltip(
                            id = "mMILink",
                            title = HTML(paste('<h5><b>',"Click Me to learn more",'</b><h6>',
                                               "Points with a large residual and high leverage have the most influence. They can have an adverse effect on the performance of the model",'<br>',
                                               "In Cooks D chart, observations in blue are normal and those red bars represent larger residuals or outliers, subject to further investigation")),
                            trigger = "hover",
                            placement = "right", 
                            options = list(container = "body"))
                          
                        )#box closure
                      ) #fluid Row Closure
                    )#column closure
                  ),#Tabpanel closure
                  tabPanel(
                    "All In One",
                    box(
                      width = 2,
                      height = '500px',
                      title = NULL,
                      align='center',
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = FALSE,
                      HTML(paste('<h5><b>',"Choose a Model",'</b>')),
                      uiOutput(outputId = 'mALLInOneID')
                    ),
                    column(
                      width = 10,
                      offset = 0,
                      fluidRow(
                        box(
                          width = 12,
                          height = '500px',
                          title = NULL,
                          align='center',
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = FALSE,
                          HTML(paste('<h5><b><CENTER>',"All In one Test of Assumptions of Linear Regression",'</CENTER></b>')),
                          uiOutput(outputId = 'mALLInOneTxt'),
                          plotOutput('mAllInOnePlot', height = '425px')
                        )#box closure
                      ) #fluid Row Closure
                    )#column closure
                  )#Tabpanel closure
      ) #tabsetpanel
    )#tabItem Closure
  )#tabItems Closure
) # dashboardBody closure



ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)





server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) #to the top of server.R would increase the limit to 30MB.
  
  options(scipen=999)  # prevents printing scientific notation
  
  inserted <- c()
  
  disable("mshowtableBtn")
  disable("mVisualizationBtn")
  disable("mFixDependentVarBtn")
  disable("mCleanseDataBtn")
  disable("mydropdown")
  
  vmy <- reactiveValues(mydata=NULL, mModelsdf=NULL)
  
  
  ############################################################
  # Creating Evaluation Criteria Table will be displayed as KBL table
  ############################################################
  Statistics <- c("R-Squared", "Adj R-Squared", "F-Statistic", "Std. Error", "t-statistic", "AIC", "BIC", "Mallows ep", "MAPE (Mean Absolute Percentage Error)", "MSE (Mean squared error)", "Min_ Max Accuracy => mean(min(actual, predicted) / max(actual,predicted))")
  Criteria <- c("Higher the better", "Higher the better", "Higher the better", "Closer to zero the better", "Should be greater 1.96 for p-value to be less than 0.05", "Lower the better", "Lower the  better", "Should be close to the number of predictors in model", "Lower the better", "Lower the better", "Higher the better")
  vmy$eval_criteriaTbl <- data.frame(Statistics,Criteria)
  
  
  
  
  #################################################
  #right left arrow for the next and previous tab  ::I got from this site; Thank you: https://stackoverflow.com/questions/44309328/generic-button-for-go-to-next-and-previous-tabitem-shiny
  #################################################
  global <- reactiveValues(tab_id = "")
  
  tab_id <- c('tab_Dataprocessing','tab_TestAssumption')
  
  Current <- reactiveValues(
    Tab = "tab_Dataprocessing"
  )
  
  
  observeEvent(
    input[["tabs"]],
    {
      Current$Tab <- input[["tabs"]]
    }
  )
  
  observeEvent(
    input[["Previous"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) - 1
      if (isTRUE(tab_id_position == 0)) tab_id_position <- length(tab_id)
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  observeEvent(
    input[["Next"]],
    {
      tab_id_position <- match(Current$Tab, tab_id) + 1
      if (isTRUE(tab_id_position > length(tab_id))) tab_id_position <- 1
      Current$Tab <- tab_id[tab_id_position]
      updateTabItems(session, "tabs", tab_id[tab_id_position]) 
    }
  )
  
  ###### end of code for go next and previous
  
  ## visualize
                          
  output$plotPrice <- renderPlot({
    data <- vmy$mydata %>% 
            group_by(CarName) %>% 
            summarize(CNT = n()) %>% 
            arrange(desc(CNT))

    ggplot(data=data,aes(x = CarName, y = CNT, group = CarName, color = CarName, fill=CarName)) +
      geom_bar(stat = "identity")+
      theme(legend.position="none") +
      scale_x_discrete(guide = guide_axis(angle = 90))

  })

  output$plotFuelGas <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(fueltype) %>% 
            count() %>% 
            ungroup() %>% 
            mutate(per=`n`/sum(`n`)) %>% 
            arrange(desc(fueltype))
    data$label <- scales::percent(data$per)
    ggplot(data=data)+
      geom_bar(aes(x="", y=per, fill=fueltype), stat="identity", width = 1)+
      coord_polar("y", start=0)+
      theme_void()+
      geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

  })

   output$plotBarFuelGas <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(fueltype) %>% 
            summarize(Mean = mean(price, na.rm=TRUE))
    ggplot(data=data,aes(x = fueltype, y = Mean))+ 
      geom_bar(stat = "identity",fill="deepskyblue3")

  })

  output$plotAspiration <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(aspiration) %>% 
            count() %>% 
            ungroup() %>% 
            mutate(per=`n`/sum(`n`)) %>% 
            arrange(desc(aspiration))
    data$label <- scales::percent(data$per)
    ggplot(data=data)+
      geom_bar(aes(x="", y=per, fill=aspiration), stat="identity", width = 1)+
      coord_polar("y", start=0)+
      theme_void()+
      geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

  })

  output$plotBarAspiration <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(aspiration) %>% 
            summarize(Mean = mean(price, na.rm=TRUE))
    ggplot(data=data,aes(x = aspiration, y = Mean))+ 
      geom_bar(stat = "identity",fill="deepskyblue3")

  })

  output$plotDoornumber <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(doornumber) %>% 
            count() %>% 
            ungroup() %>% 
            mutate(per=`n`/sum(`n`)) %>% 
            arrange(desc(doornumber))
    data$label <- scales::percent(data$per)
    ggplot(data=data)+
      geom_bar(aes(x="", y=per, fill=doornumber), stat="identity", width = 1)+
      coord_polar("y", start=0)+
      theme_void()+
      geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

  })

  output$plotBarDoornumber <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(doornumber) %>% 
            summarize(Mean = mean(price, na.rm=TRUE))
    ggplot(data=data,aes(x = doornumber, y = Mean))+ 
      geom_bar(stat = "identity",fill="deepskyblue3")

  })

  output$plotDriverWheel <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(drivewheel) %>% 
            count() %>% 
            ungroup() %>% 
            mutate(per=`n`/sum(`n`)) %>% 
            arrange(desc(drivewheel))
    data$label <- scales::percent(data$per)
    ggplot(data=data)+
      geom_bar(aes(x="", y=per, fill=drivewheel), stat="identity", width = 1)+
      coord_polar("y", start=0)+
      theme_void()+
      geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

  })

  output$plotBarDriverWheel <- renderPlot({

    data <- vmy$mydata %>% 
            group_by(drivewheel) %>% 
            summarize(Mean = mean(price, na.rm=TRUE))
    ggplot(data=data,aes(x = drivewheel, y = Mean))+ 
      geom_bar(stat = "identity",fill="deepskyblue3")

  })
  
  ##########################################################################
  #Function to remove box or column present on the screen at the placeholder
  ##########################################################################
  removeRightBox <- function(x){
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
  }
  
  
  ###################################################################
  #There is a placeholder_MultiPurpose on UI, 
  #from here, every step or action will have 3 steps; 
  #1- Insert UI with observeEvent; 
  #2- Output in that inserted UI; 
  #3- Function if any required to calculate output 
  ###################################################################
  
  observeEvent(input$mDataUploadBtn,{
    enable("mgetfileclick")
    removeRightBox()
    btn <- input$mDataUploadBtn
    id <- paste0('txt', btn)
    
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          id = "mprocBox1",
          width = 5,
          height = '500px',
          align = "center",
          title = "Uplaod Dataset",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          
          # Input: Select a file ----
          fileInput("file",
                    label = "Select: csv,txt, xls, xlsx, rds ",
                    multiple = FALSE,
                    accept = c("text/csv/txt/Excel",
                               "text/comma-separated-values,text/plain/excel",
                               ".csv",".txt",".xls",".xlsx",".rds")),
          
          # Horizontal line ----
          #tags$hr(),
          column(
            width = 5,
            offset = 1,
            align = "left",
            fluidRow(
              # Input: Checkbox if file has header ----
              checkboxInput("header", "Header", TRUE),
              
              # Input: Select separator ----
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ",")
            )
          ),
          column(
            width = 5,
            offset = 0,
            align = "left",
            fluidRow(
              br(),
              br(),
              # Input: Select quotes ----
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Qot." = '"',
                                       "Single Qot." = "'"),
                           selected = '"')
            )
          ),
          
          tags$hr(),
          column(
            width = 12,
            align="center",
            useShinyjs(),
            br(),
            actionButton(inputId = "mgetfileclick",label = "Get Data!",style = styleButtonBlue(xheight = '35px',xwidth = '200px')),
            textOutput(outputId = 'mfileimportmsg')
          )
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  observeEvent(input$mgetfileclick,{
    if (length(input$file)==0){
      shinyalert("Oops!", "Hi first browse, select and import ...!", type = "error")
      return()
    }
    enable("mshowtableBtn")
    enable("mVisualizationBtn")
    enable("mFixDependentVarBtn")
    
    #### file import code start
    ext <- tools::file_ext(input$file$name)
    
    if (ext != "csv" & ext !='rds' & ext != 'xlsx' & ext != 'xlsx'){
      shinyalert("Oops!", "valid files are csv, txt, excel and rds only", type = "error")
      return()
    }
    else if (ext == "rds"){
      vmy$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv" || ext == 'txt'){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop ::I got from this site; Thank you:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        
        vmy$mydata <- as.data.frame(
          read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote
          )
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    
    #### create datatype df to use in cleansing phase
    fncreatedftype()
    output$mfileimportmsg<-renderText({
      "Done - uploaded"
    })
    disable("mgetfileclick")
    
    updateNumericInput(session,inputId = "mVarCountFrom",min = 2,max = ncol(vmy$mydata)-1)
    disable("maddmodelYorN")
  })
  
  
  fncreatedftype <- function(){

    df_types <- data.frame("col_types" = unlist(lapply(vmy$mydata, class)))
    df_types['Var_name'] <- rownames(df_types)
    row.names(df_types) <- seq(1,nrow(df_types))
    
    dd <- data.frame(NAs=sapply(vmy$mydata, function(x) sum(is.na(x))))
    dd['Var_name'] <- rownames(dd)
    row.names(dd) <- seq(1,nrow(dd))
    
    df_types <- plyr::join(x = df_types,y = dd,by= 'Var_name')
    
    vmy$df_types <- df_types
    vmy$df_types <-vmy$df_types %>% dplyr::select('Var_name', everything())      
  }
  
  
  
  
  #####################################################################################
  # Show data table 
  #####################################################################################
  observeEvent(input$mshowtableBtn,{
    removeRightBox()
    btn <- input$mshowtableBtn
    id <- paste0('txt', btn)
    disable("maddmodelYorN")
    enable("mFixDependentVarBtn")
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          width = 7,
          height = '500px', 
          title ="Dataset", 
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          DT::dataTableOutput('mdatatable', height = '400px',width = '100%')
        ), #box closure,
        box(
          width = 5,
          class = 'boxHandleData',         
          title = "Data Cleansing",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          actionButton("deleteRows", "Delete Row", style = styleButtonBlue(xcolor = 'pink')),
          actionButton("deleteRowEmpty", "Remove empty rows", style = styleButtonBlue(xcolor = '#FF9632')),
          br(),
          radioGroupButtons(
            inputId = "mRadioNABtn",
            label = "Replace NAs with",
            choices = c("ZERO", "MEAN", "MEDIAN", "MODE"),
            selected = character(0),
            status = "primary",
            checkIcon = list(
              yes = icon("ok",
                        lib = "glyphicon"),
              no = icon("xmark",
                        lib = "glyphicon"))
          ),
          DT::dataTableOutput("dt",height = '275px'),
          tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
          useShinyjs(),
          extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
          tags$br(),
          textOutput("mselectedvariable"),
          actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable!",style = styleButtonBlue(xheight = '35px')),
          br(),
          
        ) #box closure
      )
    )
    inserted <<- c(id, inserted)
  })
  
  output$mdatatable <- DT::renderDataTable({
    DT::datatable(vmy$mydata,  
                  editable = TRUE,
                  rownames = FALSE,
                  selection = list(mode = "single", selected = c(1), target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(scrollY = '325px',   #where you got scrollY parameter: https://stackoverflow.com/questions/38832890/datatable-to-resize-with-window-in-shiny   ## some more parameters: https://datatables.net/reference/option/
                                 lengthMenu = c(10, 30, 50), 
                                 pageLength = 10,
                                 initComplete = htmlwidgets::JS(
                                   "function(settings, json) {",
                                   paste0("$(this.api().table().container()).css({'font-size': '", "12px", "'});"),
                                   "}")
                  ),
                  class ='cell-border stripe compact white-space: nowrap' #::I got from this site; Thank you this multiple classes: https://rstudio.github.io/DT/
    ) 
  })
  
  
  
  #############################################################################
  # here you get option to delete columns that are not required, for example ID  
  #############################################################################
  observeEvent(input$deleteRows,{
    
    if (!is.null(input$mdatatable_rows_selected)) {
      
      vmy$mydata <- vmy$mydata[-as.numeric(input$mdatatable_rows_selected),]
    }
    fncreatedftype()
  })

  observeEvent(input$deleteRowEmpty,{
      #### delete NA rows code start
      vmy$mydata <- na.omit(vmy$mydata)
      vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
      fncreatedftype()
  })
  observeEvent(input$mCleanseDataBtn,{
    enable("mFixDependentVarBtn")
    removeRightBox()
    btn <- input$mCleanseDataBtn
    id <- paste0('txt', btn)
    
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          id = "mprocBox2",
          width = 5,
          height = '500px',
          align = "center",
          title = "Data Cleansing",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          DT::dataTableOutput("dt",height = '275px'),
          tags$style(HTML('table.dataTable tr.selected td{background-color: pink !important;}')),
          useShinyjs(),
          extendShinyjs(text = paste0("shinyjs.resetDTClick = function() { Shiny.onInputChange('dt_cell_clicked', null); }"),functions = c('foo','bar')),
          tags$br(),
          textOutput("mselectedvariable"),
          actionButton(inputId = 'mbtndelete',label = "Delete Selected Variable!",style = styleButtonBlue(xheight = '35px',xwidth = '200px'))
        )
      )
    )
    inserted <<- c(id, inserted)
  })
  
  # replace NA value
  observeEvent(input$mRadioNABtn,{
    if (length(input$file)==0){
      return()
    }
    
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure You want to update NA values in :",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mNAsok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
    
  })

  # Delete all Empty Rows
  observeEvent(input$mbtnEmptyRows,{
    if (length(input$file)==0){
      return()
    }
    showModal(
      modalDialog(
        title = "Warning",
        paste("Are you sure to delete all Empty Rows" ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mEmptyRowsyes", "Yes")
        ), easyClose = TRUE)
      
    )
  })

  ### If user say OK, then Character the selected rows
  observeEvent(input$mEmptyRowsyes, {
    vmy$mydata <- vmy$mydata[rowSums(is.na(vmy$mydata)) != ncol(vmy$mydata), ]        # Drop empty rows
    
    fncreatedftype()
    removeModal()
  })
  
  observeEvent(input$mNAsok, {
    nn <- nrow(vmy$mydata)
    clicked_list <- input$dt_rows_selected
    mselected <- vmy$df_types[clicked_list,1]
    
    if (input$mRadioNABtn == "ZERO"){
      for (i in mselected){
        mreplaceval <- 0
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval

        }
      }
    }else if (input$mRadioNABtn == "MEAN"){
      for (i in mselected){
        mreplaceval <- mean(vmy$mydata[,i],na.rm	= TRUE )
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval
        }
      }
    }else if (input$mRadioNABtn == "MEDIAN"){
      for (i in mselected){
        mreplaceval <- median(vmy$mydata[,i],na.rm	= TRUE )
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval
        }
      }
    }else if (input$mRadioNABtn == "MODE"){
      for (i in mselected){
        mreplaceval <- getmode(vmy$mydata[,i] )
        for (r in (1:nn)){
          vmy$mydata[r,i][is.na(vmy$mydata[r,i])] <- mreplaceval
        }
      }
    }
    fncreatedftype()
    removeModal() 
    updateRadioGroupButtons(session,inputId = 'mRadioNABtn', selected = character(0))
  })
  
  output$dt <- DT::renderDataTable({
    DT::datatable(vmy$df_types,
                  rownames = FALSE,
                  width = NULL,
                  height = NULL,
                  editable = FALSE,
                  selection = list(mode = "single", target = 'row'),
                  fillContainer = getOption("DT.fillContainer", TRUE),
                  options = list(scrollY = '275px',dom = 't',ordering=F, pageLength = -1,class="compact",
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#808080', 'color': '#fff'});",   #::I got from this site; Thank you: JS from https://stackoverflow.com/questions/34009922/how-to-format-a-shiny-rendertable
                                   "}")
                  ) 
    )
    
  })
  
  ### delete selected column
  ### this is warning messge for deleting
  observeEvent(input$mbtndelete,{
    showModal(
      if(length(vmy$df_types[input$dt_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete variable:",vmy$df_types[input$dt_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the variable that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    temp <- select(vmy$mydata,-vmy$df_types[input$dt_cell_clicked$row,1])
    vmy$mydata <- temp
    
    dd <-which((names(vmy$mydata) %in% input$mPickerDepenVar)==TRUE)
    mcheckboxchoices <- names(vmy$mydata)[-dd]
    updatePickerInput(session,inputId = "mPickerIndepVar",choices = mcheckboxchoices ,selected = NULL)
    
    fncreatedftype()
    removeModal()
    temp2 <- subset(vmy$df_types, Var_name!=vmy$df_types[input$dt_cell_clicked$row,1] )
    vmy$df_types <- temp2
  })
  
  
  
  output$mselectedvariable <-  renderText({
    if(length(input$dt_cell_clicked) != 0){
      clicked_list <- input$dt_cell_clicked
      HTML(paste("selected:",vmy$df_types[clicked_list$row,1],"Type:",vmy$df_types[clicked_list$row,2]))
      
    }
    else{
      HTML("select variable")
    }
  })
  

  #####################################################################################
  # Visualization
  #####################################################################################

  observeEvent(input$mVisualizationBtn,{
    removeRightBox()
    btn <- input$mVisualizationBtn
    id <- paste0('Visualization', btn)
    disable("maddmodelYorN")
    enable("mFixDependentVarBtn")
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,

        box(
          "Number of occurrences of vehicles in the Dataset", 
          width = 12, 
          class="plot", 
          plotOutput("plotPrice"), 
          "The famous or repeats car > Toyota"
        ),
        box(
          "Show which more using Fuel Type ", 
          width = 6, 
          class="plot", 
          plotOutput("plotFuelGas")
        ),
        box(
          "Show which more using Fuel Type ", 
          width = 6, 
          class="plot", 
          plotOutput("plotBarFuelGas"),
          "Most using car working in Gas around 90 % and the average price less than the car working in Diesel.
            Some category using the car working in Diesel around 10 % but the average price more than the car working in Gas."),
        box(
          "Which is most Aspiration repeats in the Dataset [ car Standard or car Turbo ]", 
          width = 6, 
          class="plot", 
          plotOutput("plotAspiration")
          ),
        box(
          "Which is most Aspiration repeats in the Dataset [ car Standard or car Turbo ]", 
          width = 6, 
          class="plot", 
          plotOutput("plotBarAspiration"),
          "Most cars Stander around 82% also the average price less Turbo, the percentage turbo car around 18 %."
        ),
        box(
          "Which is most Door Number repeats in the Dataset [2 or 4]", 
          width = 6, 
          class="plot", plotOutput("plotDoornumber")
          ),
        box(
          "Which is most Door Number repeats in the Dataset [2 or 4]", 
          width = 6, 
          class="plot", 
          plotOutput("plotBarDoornumber"),
          "Around 56 % using car has 4 doors and 46% using car has 2 doors, sports car also the same average price."
        ),
        box("Which is most Car Drive Wheel repeats in the Dataset ", width = 6, class="plot", plotOutput("plotDriverWheel")),
        box(
          "Which is most Car Drive Wheel repeats in the Dataset ", 
          width = 6, 
          class="plot", 
          plotOutput("plotBarDriverWheel"),
          "Most people using type [Drive Wheel ]> fwd , I thing because less price"
        ),
      
      )
    )
    inserted <<- c(id, inserted)
  })
  
  
  
  #####################################################################################
  # select dependent ,independent variables and model selection
  #####################################################################################
  observeEvent(input$mFixDependentVarBtn,{
    disable(id = 'mPickerIndepVarBtn')
    removeRightBox()
    
    if(!exists("vmy$mModelsdf")){
      vmy$mModelsdf <- data.frame(Dependent_ = as.character(),
                                  Predictor_     = as.character()
      )
      vmy$tempdf2   <- vmy$mModelsdf 
      
    }
    
    btn <- input$mFixDependentVarBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder_MultiPurpose',
      where = "beforeBegin",
      multiple = FALSE,
      ui = tags$div(
        id = id,
        box(
          id = "mprocBox3",
          width = 12,
          height = '500px',
          align = "center",
          title = "Variable Selection",
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          column(
            width = 4,
            actionButton(inputId = "mPickerDepenVarBtn", label = "Choose Dependent variable & Click Me",style=styleButtonBlue(xheight = '60px',xwidth = '150px')),
            
            selectInput(inputId = 'mPickerDepenVar',label = NULL,
                        choices =names(vmy$mydata),selected = rev(names(vmy$mydata))[1] )
          ),
          column(
            width = 4,
            radioGroupButtons(
              inputId = "radioreplyRegType",
              label = "Choose Regression Type",
              choices = c("Simple","Multiple"),
              individual = TRUE,
              selected = c("Multiple"),
              size = "sm", #size options are "xl", "sm","normal","lg"
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle",
                             style = "color: red"),
                no = tags$i(class = "fa fa-circle-o",
                            style = "color: steelblue"))
            )# radioGroupbtn closure
          ),
          column(
            width = 4,
            actionButton(inputId = "mPickerIndepVarBtn", label = "Choose Independent variables & Click Me",style=styleButtonBlue(xheight = '60px',xwidth = '150px')),
            pickerInput(
              inputId = "mPickerIndepVar",label = NULL,multiple = TRUE, choices = NULL,selected = NULL,
              options = list('actions-box' = TRUE)
            )
          ),
          column(
            width = 12, 
            DT::dataTableOutput(outputId = 'mChosenModelTbl',width = '100%',height = '250px'),
            #style = "height:350px; overflow-y: scroll;",
            tags$br(),tags$br(),
            splitLayout(
              cellWidths = c('20%','60%','20%'),
              actionButton(inputId = "mClearTableBtn", label = "Clear Entire Table",style=styleButtonBlue(xheight = '35px',xwidth = '150px',xcolor = '#FF7900')),
              
              HTML(paste('<h6><b>','Max 10 models are only allowed to give better clarify on multiple charts','</b><h5>')),
              actionButton(inputId = "mDeleteCriteriaBtn", label = "Delete Selected Row",style=styleButtonBlue(xheight = '35px',xwidth = '150px',xcolor = '#FF7900')),
              
            )
          )
          
        )
      )
    )
    inserted <<- c(id, inserted)
    enable("mydropdown")
    
  })
  
  
  
  observeEvent(input$mPickerDepenVarBtn, {
    #### file import code End 
    vmy$mydata  <- dplyr::select_if(vmy$mydata,is.numeric) 
    vmy$mydata  <- cbind(vmy$mydata, dplyr::select_if(vmy$mydata,is.factor))
    if (length(input$mPickerDepenVar)==0){
      alert("variables not selected please check...!")
      return()
    }
    
    vmy$mydata <-vmy$mydata %>% dplyr::select(-input$mPickerDepenVar, everything())
    
    dd <-which((names(vmy$mydata) %in% input$mPickerDepenVar)==TRUE)
    mcheckboxchoices <- names(vmy$mydata)[-dd]
    updatePickerInput(session,inputId = "mPickerIndepVar",choices = mcheckboxchoices ,selected = NULL)
    
    fncreatedftype()
    click(id = 'radioGroupButtons')
    enable(id = 'mPickerIndepVarBtn')
  })
  
  observeEvent(input$mPickerDepenVar,{
    disable(id = 'mPickerIndepVarBtn')
  })
  
  observeEvent(input$mPickerIndepVarBtn,{
    if (length(input$mPickerDepenVar)==0 | length(input$mPickerIndepVar)==0 ){
      alert("variables not selected please check...!")
      return()
    }
    if(nrow(vmy$mModelsdf)>=10){
      shinyalert("Max 10 models can you be analyzed at a time")
      return()
    }
    
    if (paste0(input$mPickerIndepVar,collapse = ' + ') %in% c(vmy$mModelsdf$Predictor_) ){
      alert("This has already been added to Model table below")
      return()
    }
    
    if (input$radioreplyRegType=='Simple'){
      if(length(input$mPickerIndepVar)>10){
        shinyalert("Max 10 variables can only be added")
        n <- 10
      }
      else{
        n <- length(input$mPickerIndepVar)
      }
      
      r <- nrow(vmy$mModelsdf)
      vmy$mModelsdf <-vmy$mModelsdf[-c(1:r),]
      vmy$mModelsdf <- vmy$mModelsdf %>%
        add_row(
          Dependent_ = input$mPickerDepenVar,
          Predictor_ = input$mPickerIndepVar[1:n]
        ) 
    }
    else {
      if(nrow(vmy$mModelsdf)>=10){
        shinyalert("Max 10 models can you be analyzed at a time")
        return()
      }
      vmy$mModelsdf <- vmy$mModelsdf %>%
        add_row(
          Dependent_ = input$mPickerDepenVar,
          Predictor_ = paste0(input$mPickerIndepVar,collapse = ' + ')
        )
    }
    
    
    vmy$tempdf2 <- vmy$mModelsdf   # this is used in dropdown menu on the top 
    dd <-which((names(vmy$mydata) %in% input$mPickerDepenVar)==TRUE)
    mcheckboxchoices <- names(vmy$mydata)[-dd]
    updatePickerInput(session,inputId = "mPickerIndepVar",choices = mcheckboxchoices ,selected = NULL)
    
    output$mALLInOneID <- renderUI({
      pickerInput(
        inputId = "mSelectedModelAllInOneTab",
        label = NULL,
        multiple = FALSE,
        choices = c(vmy$tempdf2$Predictor_),
        selected = vmy$tempdf2$Predictor_[1],
        options = list('actions-box' = TRUE)
      )
    })
    
    
    fnNorDisStatisticdf()   # this to create normal distribution test statistics data table
    fnShowFlashCard () #flashcard function
  })
  
  
  
  output$mChosenModelTbl <- DT::renderDataTable({
    DT::datatable(vmy$tempdf2,
                  class ='cell-border stripe compact white-space: nowrap', #::I got from this site; Thank you this multiple classes: https://rstudio.github.io/DT/
                  escape=F,
                  editable = F,
                  rownames = T,
                  selection = list(mode = "single", target = 'row'),
                  options = list(scrollY = '250px',
                                 dom = 't',
                                 ordering=F,
                                 pageLength = -1,
                                 class="compact",
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width ='5px', targets = c(0)),
                                                   list(width ='20px', targets = c(1)),
                                                   list(width ='100%',targets = c(2)))
                                 
                  ) ,
                  
    ) %>% my.styleallrows()
  })
  
  my.styleallrows <- function(.) formatStyle(., columns=0, target= 'row',color = 'black',
                                             backgroundColor = '#ffffed',
                                             fontWeight ='normal',lineHeight='90%')
  
  
  
  
  
  ###############################################
  ### select and delete models
  ###############################################
  observeEvent(input$mDeleteCriteriaBtn,{
    showModal(
      if(length(vmy$mModelsdf[input$mChosenModelTbl_cell_clicked$row,1])>0 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure delete the Model:",vmy$mModelsdf[input$mChosenModelTbl_cell_clicked$row,1] ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("mModelok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select the Row that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$mModelok, {
    removeModal()
    
    temp2 <- vmy$tempdf2[-c(input$mChosenModelTbl_cell_clicked$row),]
    vmy$tempdf2 <- temp2   # this is used in dropdown menu on the top 
    vmy$mModelsdf <- temp2
    
    mcheckboxchoices <- c(vmy$tempdf2$Predictor_)
    updatePickerInput(session,inputId = "mSelectedModel",choices = mcheckboxchoices ,selected = mcheckboxchoices)
    updatePickerInput(session,inputId = "mSelectedModelAllInOneTab",choices = mcheckboxchoices,selected = mcheckboxchoices[1])
    updatePickerInput(session,inputId = "mPickerDepenVar",choices = names(vmy$mydata) ,selected = NULL)
    
    if (length(vmy$tempdf2$Predictor_)>0){
      fnNorDisStatisticdf()   # this to create normal distribution test statistics data table
      fnShowFlashCard () #flashcard function
    }
    else{
      #here we delete all entries from the statistical test of normality of residuals
      n <- nrow(vmy$normalstatdf)
      vmy$normalstatdf <- vmy$normalstatdf[-c(1:n),]
    }
    
  })
  
  
  ###############################################
  ### Clear completely model selected table
  ###############################################
  observeEvent(input$mClearTableBtn,{
    showModal(
      modalDialog(
        title = "Warning",
        paste("Do you want to clear all entries in table ?" ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("mModelClearok", "Yes")
        ), easyClose = TRUE)
      
    )
  })
  
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$mModelClearok, {
    removeModal()
    
    n <- nrow(vmy$tempdf2)
    vmy$tempdf2 <- vmy$tempdf2[-c(1:n),]
    vmy$mModelsdf <-  vmy$tempdf2
    
    
    mcheckboxchoices <- c(vmy$tempdf2$Predictor_)
    updatePickerInput(session,inputId = "mSelectedModel",choices = mcheckboxchoices ,selected = mcheckboxchoices)
    updatePickerInput(session,inputId = "mSelectedModelAllInOneTab",choices = mcheckboxchoices,selected = mcheckboxchoices[1])
    
    
    #here we delete all entries from the statistical test of normality of residuals
    n <- nrow(vmy$normalstatdf)
    vmy$normalstatdf <- vmy$normalstatdf[-c(1:n),]
    
    disable(id = 'mPickerIndepVarBtn')
  })
  
  
  ####################################################
  #Dropdown Modal Dialog button to choose model
  ####################################################
  observeEvent(input$mydropdown,{
    mcheckboxchoices <- c(vmy$tempdf2$Predictor_)
    mxalreadyselected <- input$mSelectedModel
    
    showModal(
      modalDialog(
        size = 's',
        column(
          width=12,
          align = "center",
          HTML(paste('<h5><b>',"Choose Model(s)",'</b>')),
          pickerInput(
            inputId = "mSelectedModel",
            label = NULL,
            multiple = TRUE,
            choices = mcheckboxchoices,
            selected = ifelse(length(mxalreadyselected)>0,mxalreadyselected,mcheckboxchoices),
            options = list('actions-box' = TRUE)
          )
        ), #column
        easyClose = TRUE
      )
    )
    if (length(mxalreadyselected) >0){
      updatePickerInput(session,inputId = "mSelectedModel",choices = mcheckboxchoices ,selected = mxalreadyselected)
      
    }
    else{
      updatePickerInput(session,inputId = "mSelectedModel",choices = mcheckboxchoices ,selected = mcheckboxchoices)
      
    }
    
  })
  
  
  observeEvent(input$mSelectedModel,{
    if (length(vmy$eval_criteriaTbl)==0){
      return()
    }
    else{
      vmy$mModelsdf <- vmy$tempdf2[vmy$tempdf2$Predictor_ %in% input$mSelectedModel, ]
      fnNorDisStatisticdf()   # this to create normal distribution test statistics data table
      fnShowFlashCard () #flashcard function
    }
  })
  
  
  
  
  ####################################################
  #Testing Assumption:  Linearity
  ####################################################
  
  output$mLinearityPlot <-renderPlot({
    theme_set(
      theme_grey(12)
    )
    
    n <- ncol(vmy$mydata)-1
    c <- ifelse (n>2,ceiling(n/2),n)
    r <- ifelse (n<= 2,1,2)
    par(mfrow=c(r,c)) #get in 3 rows and 3 columns (first one is row)
    #par("mar") (bottom, left, top, right) in lines
    #par(mar=rep(0,4))
    
    my_plots <- list()
    my_text <- ""
    
    library(gridExtra)
    for(i in 1:n) {
      mcortest <- cor.test(x = vmy$mydata[,i],y = vmy$mydata[,input$mPickerDepenVar],method='pearson')
      mcorcoff <- round(mcortest$estimate,2)
      mtstat   <- round(mcortest$statistic,2)
      mtstatp  <- round(mcortest$p.value,3)
      mtext    <- paste("Corr Coeff:",mcorcoff,";p-value of t-stat:",mtstatp)
      my_plots[[i]] <- fnGenerateLinearity(i,mtext)
      my_text  <-paste(my_text,'<h6><b>','&nbsp;',i,'.',names(vmy$mydata[i]),":",'</b>',"p-value is ",mtstatp,
                       ifelse(mtstatp>0.05,paste(", which is > 0.05; conclusion:",'<b><FONT COLOR="#E51616">',"fail to reject, NO Linearity.",'</b><FONT COLOR="#000000">'),
                              paste(", which is < 0.05; conclusion:",'<b><FONT COLOR="#339933">',"reject NULL; Linearity is present",'</b><FONT COLOR="#000000">')),'\n')
      
    }
    vmy$mLinearityTxt <- my_text
    grid.arrange(grobs =my_plots,nrow=r, ncol=c)
  })
  
  
  
  fnGenerateLinearity <- function(i,mtext){
    mfontsize <- ifelse(ncol(vmy$mydata)>4,10,12)
    
    ggplot(vmy$mydata, aes(x = vmy$mydata[,i],y = vmy$mydata[,input$mPickerDepenVar])) +
      geom_point() +
      geom_smooth(method = lm, se = TRUE,color='red') +
      labs(subtitle=mtext, y=input$mPickerDepenVar,
           x=names(vmy$mydata[i]))+
      theme_gray(base_size = mfontsize)+
      theme(
        legend.position  = 'none',
        text = element_text(color = "black", size = mfontsize,face = "italic"),complete = TRUE
      )
  }
  
  
  observeEvent(input$mLinearityLink,{
    urlassumption1 <- a("Correlation Coefficient by Jason Fernando on Investopedia", href="https://www.investopedia.com/terms/c/correlationcoefficient.asp")
    urlassumption1A <- a("What Is Pearson Correlation? Including Test Assumptions on toptipbio.com", href="https://toptipbio.com/what-is-pearson-correlation/")
    showModal(
      modalDialog(
        size = 'm',
        align='center',
        title = "Pearson Correlation Coefficient",
        align='left',
        HTML(paste('<h6><b>',"Intro:",'</b>',
                   "Correlation Coefficient measures the strength/degree and direction of the linear relationship between two variables.",
                   "Here we use Pearson correlation coefficient. The correlation coefficient always range between -1",
                   "and +1. Values at or close to zero imply a weak or no linear relationship.",
                   "This test provides us t-statistics, associated p-value and correlation cofficient.")),
        HTML(paste('<h6><b>',"Hypotheses-Pearson Corr. Coeff.:",'</b><br>',
                   "H0 (null hypothesis): Correlation is Zero and NO linear relationship  between two variables.",'<br>',
                   "HA (alternative hypothesis): There exists correlation and linear relationship ",'<br>',
                   '<h6><b>',"Interpretation-Pearson Corr. Coeff Test Results(two-sided analysis):",'</b><br>',
                   "In general, an absolute value of correlation coefficient more than 0.8 is considered to be significant, and the assumption of linearity is met",
                   "Further, if p-value associated with test statistics is greater than 0.05, we fail to reject NULL hypothesis; we conclude that there is NO correlation among these two variables.",
                   "On the other hand, if less than 0.05, we reject the NULL hypothesis and there exists correlation and linear relationship.",'<br><br>',
                   "For our dataset, Pearson Correlation Coefficient, p-value of t-statistics and interpretation of p-value",'<br>',
                   vmy$mLinearityTxt)),
        footer = HTML(paste('<h6><b><CENTER>',"Reference 1:",urlassumption1,'<br>',
                            "Reference 2:",urlassumption1A,'</CENTER></b><h6>')),
        easyClose = TRUE)
    )
    
  })
  
  
  ####################################################
  #Testing Assumption:  Multicollinearity
  ####################################################
  output$predictorMulticollinearity <- renderPrint({
    f2 <- as.formula(paste(paste( text=input$mPickerDepenVar,"~"), "."))
    vmy$fullMod <- lm(f2,  data=vmy$mydata)
    
    if (ncol(vmy$fullMod$model)>2){
      car::vif(vmy$fullMod)
    }
    else{
      HTML("variable influence cannot be calculated with one predictor..!")
    }
    
  })
  
  output$predictorMulticollinearityTxt <- renderUI({
    ccdf <- as.data.frame(car::vif(vmy$fullMod))
    m5_0corrvar <- rownames(ccdf%>% dplyr::filter(ccdf[1]>5.0))
    m2_5corrvar <- rownames(ccdf%>% dplyr::filter(ccdf[1]>2.5))
    fluidRow(
      column(
        width = 6,
        tags$div(
          tags$p(
            useShinyjs(),
            HTML(paste('<h5><b>',"Interpreting the Variance Inflation Factor (VIF):",'</b>',"as explained in the article referred here",'<br><h5>')),
            HTML(paste('&nbsp','&nbsp','&nbsp',"Variables having correlation > 5.0:")),
            
            if (length(m5_0corrvar)!=0){
              HTML(paste(rownames(as.data.frame(car::vif(vmy$fullMod))[1]%>% dplyr::filter(as.data.frame(car::vif(vmy$fullMod))[1]>5.0))))
            }
            else{
              HTML("NULL")
            },
            
            HTML(paste('<br>','&nbsp','&nbsp','&nbsp',"Variables having correlation > 2.5:")),
            if (length(m2_5corrvar)!=0){
              HTML(paste(rownames(as.data.frame(car::vif(vmy$fullMod))[1]%>% dplyr::filter(as.data.frame(car::vif(vmy$fullMod))[1]>2.5))))
            }
            else{
              HTML("NULL")
            },
            urlvif,
            HTML(paste('<h5>',"Variance inflation factors range from 1 upwards. The numerical value for VIF tells you (in decimal form) what",
                       "percentage the variance (i.e. the standard error squared) is inflated for each coefficient. For example, a VIF of 1.9",
                       "tells you that the variance of a particular coefficient is 90% bigger than what you would expect if there was no",
                       "multicollinearity - if there was no correlation with other predictors."))
          )
        )),
      column(
        width = 6,
        tags$div(
          tags$p(
            HTML(paste('<h5><b>',"A rule of thumb for interpreting the variance inflation factor:",'</b><br><h5>')),
            HTML(paste('<h5>','&nbsp','&nbsp','&nbsp',"1 = not correlated. ",
                       '<br>','&nbsp','&nbsp','&nbsp',"Between 1 and 5 = moderately correlated.",
                       '<br>','&nbsp','&nbsp','&nbsp',"Greater than 5 = highly correlated.")),
            br(),
            HTML(paste('<h5>',"Exactly how large a VIF has to be before it causes issues is a subject of debate. What is known is that the more your",
                       "VIF increases, the less reliable your regression results are going to be. In general, a VIF above 10 indicates high",
                       "correlation and is cause for concern. Some authors suggest a more conservative level of 2.5 or above."))
          )
        )
      )
    )
  })
  
  
  observeEvent(input$mMulticollinearityLink,{
    urlassumption2 <- a("Multicollinearity: How does it affect to your regression model? on medium.com by hqtquynhtram", href="https://medium.com/@hqtquynhtram/multicollinearity-how-does-it-affect-to-your-regression-model-e1b217b9a24b")
    showModal(
      modalDialog(
        size = 'm',
        align='center',
        title = "Multicollinearity & How to handle",
        align='left',
        HTML(paste('<h6><b>',"Intro:",'</b><br>',
                   "Multicollinearity, if exists in your model, it means some independent variables are correlated, it indicates that changes in one variable are associated with shift in another variable.")),
        HTML(paste('<h6><b>',"How to handle Multicollinearity:",'</b>',"as detailed in the articled refered in the footer",'<br>',
                   "Some recommended method to remove or reduce Multicollinearity in your linear regression model",'<br>',
                   "1. Hold out a variable and remove other variables highly correlated with the hold-out variable",'<br>',
                   "2. Linearly combine correlated variables together",'<br>',
                   "3. PCA on highly correlated independent variables",'<br>',
                   "4. LASSO or Ridge Regression are advance forms of regression analysis that can handle multicollinearity",'<br>'
        )),
        footer = HTML(paste('<h6><b><CENTER>',"Reference:",urlassumption2,'</CENTER></b><h6>')),
        easyClose = TRUE)
    )
    
  })
  
  
  ####################################################
  #Testing Assumption:  Residuals Independence
  ####################################################
  output$mIndependenceTxt1 <-renderUI({
    tags$div(
      tags$p(
        useShinyjs(),
        HTML(paste('<h6><b>',"Intro:",'</b><br>',
                   "One of the main assumptions in linear regression is that there is no correlation between consecutive residuals,i.e. residuals are independent.",'<br>',
                   '<h6><b>',"Durbin-Watson (D-W) test",'</b>', "is used to detect the presence of autocorrelation in the residuals of a regression.",
                   "Auto correlation is a characteristic of data which shows the degree of similarity between the values of the same variables over successive time intervals."),'<br>'),
        HTML(paste('<h6><b>',"Hypotheses in D-W Test:",'</b><br>',
                   "H0 (null hypothesis): There is no correlation among the residuals.",'<br>',
                   "HA (alternative hypothesis): The residuals are autocorrelated.",'<br>',
                   '<h6><b>',"Interpretation of D-W Test Results:",'</b><br>',
                   "If p-value associated with test statistics is greater than 0.05, we fail to reject NULL hypothesis; we can safely conclude that NO correlation among the residuals.",
                   "On the other hand, if less than 0.05 that the residuals in this regression model are autocorrelated."))
        
      )
    )
  })
  
  
  output$mIndependenceTxt2 <-renderUI({
    library(car)
    n <- nrow(vmy$mModelsdf)
    dwtTxt <- ""
    for(i in 1:n) {
      f1 <- as.formula(paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i]))
      model <- lm(f1, data = vmy$mydata)
      dwt <- durbinWatsonTest(model)
      mtstat <- dwt$dw
      mtstatp <- round(dwt$p,3)
      modeltext <- paste(paste(i,'.',text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i])
      dwtTxt <- paste(dwtTxt,'<h6><b>',modeltext,":",'</b>',"p-value is ",mtstatp,
                      ifelse(mtstatp<0.05,paste("which is < 0.05; conclusion:",'<b><FONT COLOR="#E51616">',"the residuals are autocorrelated.",'</b><FONT COLOR="#000000">'),
                             paste("which is > 0.05; conclusion:",'<b><FONT COLOR="#339933">',"NO correlation among the residuals.",'</b><FONT COLOR="#000000">')),'\n')
      
    }
    
    tags$div(
      tags$p(
        useShinyjs(),
        HTML(paste('<h6><b>',"Interpretation of Durbin-Watson Test Results:",'</b><h6>',dwtTxt))
        
      )
    )
  })
  
  
  ####################################################
  #Testing Assumption:  Residuals Homoscedasticity
  ####################################################
  output$mHomoscedasticityPlot <-renderPlot({
    theme_set(
      theme_grey(12)
    )
    
    n <- nrow(vmy$mModelsdf)
    c <- ifelse (n>2,ceiling(n/2),n)
    r <- ifelse (n<= 2,1,2)
    
    par(mfrow=c(r,c)) #get in 3 rows and 3 columns (first one is row)
    my_plots <- list()
    my_text <- ""
    my_subtitle <- ""
    library(gridExtra)
    for(i in 1:n) {
      f2 <- as.formula(paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i]))
      model <- lm(f2, data = vmy$mydata)
      
      mbptest <- lmtest::bptest(model)
      mtstat   <- round(mbptest$statistic,2)
      mtstatp  <- round(mbptest$p.value,3)
      my_subtitle <- paste("Breush-Pagan test:",
                           ifelse(mtstatp>0.05,"Homoscedasticity present","NO Homoscedasticity"))
      
      xdependent <- vmy$mModelsdf$Dependent_[i]
      xpredictor <-  vmy$mModelsdf$Predictor_[i]
      my_text  <-paste(my_text,paste('<h6><b>','&nbsp;',i,'.',paste(xdependent,"~",xpredictor),":",'</b>',"p-value is ",mtstatp,
                                     ifelse(mtstatp>0.05,paste(", which is > 0.05; conclusion:",'<b><FONT COLOR="#339933">',"fail to reject, Homoscedasticity is present.",'</b><FONT COLOR="#000000">'),
                                            paste(", which is < 0.05; conclusion:",'<b><FONT COLOR="#E51616">',"reject NULL; Homoscedasticity NOT present",'</b><FONT COLOR="#000000">')),'\n'))
      
      mtxtcolor <- ifelse(mtstatp>.05,"black","red")
      
      fnGenerateHomoscedasticity(i,model,my_subtitle,mtxtcolor,xdependent,xpredictor)
    }
    vmy$HomoTxt <- my_text
    
  })
  
  
  fnGenerateHomoscedasticity <- function(i,model,my_subtitle,mtxtcolor,xdependent,xpredictor){
    mformulatxt <- paste(paste( text=xdependent,"~"), xpredictor)
    plottitle <- paste("Residuals vs Fitted ::",paste0("(",mformulatxt,")") )
    if (nrow(vmy$mModelsdf)>6){
      plottitle <- paste(stringr::str_wrap(plottitle,width = 35), sep = '')      
    }
    
    plot(model,which=1,
         caption = plottitle,
         cex.caption = ifelse(nrow(vmy$mModelsdf)>4,0.8,1.0))   #where you got this: https://rdrr.io/r/stats/plot.lm.html#heading-7
  }
  
  observeEvent(input$mHomoLink,{
    urlassumption3 <- a("Statistical tests for heteroscedasticity The Breush-Pagan test on Rbloggers.com", href="https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/")
    urlassumption3A <- a("How to Interpret Diagnostic Plots in R at statology.org", href="https://www.statology.org/diagnostic-plots-in-r/")
    
    showModal(
      modalDialog(
        size = 'm',
        align='center',
        title = "Homoscedasticity",
        align='left',
        
        HTML(paste('<h6><b>',"Intro:",'</b>',
                   "The next assumption is that the residuals have constant variance at every level of x or the residuals are evenly distributed at each level of the response variable.",
                   "This is known as homoscedasticity.","When homoscedasticity is not present; the results of the regression analysis become hard to trust","<br>",
                   "The Breush-Pagan test is used,here, to determine whether heteroscedasticity is present in a regression model or not.",'<br>',
                   '<h6><b>',"Hypothesis:",'</b>', "Ho-Homoscedasticity is present. and HA-Homoscedasticity is NOT present.",'<br>',
                   '<h6><b>',"Interpretation:",'</b>', "This BP test provides test statistics and p-value of test statistics.",
                   "if the p-value is > 0.05, we fail to reject NULL hypothesis and as such Homoscedasticity is present and the assumption is met",'<br><br>',
                   vmy$HomoTxt)
        ),
        
        footer = HTML(paste('<h6><b><CENTER>',"Reference 1:",urlassumption3,'<br>',
                            "Reference 2:",urlassumption3A,'</CENTER></b><h6>')),
        easyClose = TRUE)
    )
    
  })
  
  
  ######################################################
  #Testing Assumption:  Residuals Normality with QQ Plot
  ###################################################### 
  output$mQQNormalityPlot <-renderPlot({
    theme_set(
      theme_grey(12)
    )
    library(gridExtra)
    n <- nrow(vmy$mModelsdf)
    c <- ifelse (n>2,ceiling(n/2),n)
    r <- ifelse (n<= 2,1,2)
    
    par(mfrow=c(r,c)) #get in 3 rows and 3 columns (first one is row)
    my_plots <- list()
    my_text <- ""
    mformulatxt <- ""
    for(i in 1:n) { 
      f2 <- as.formula(paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i]))
      model <- lm(f2, data = vmy$mydata)
      
      mformulatxt    <- paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i])
      
      plottitle <- paste("Normal Q-Q plot ::",paste0("(",mformulatxt,")") )
      if (nrow(vmy$mModelsdf)>6){
        plottitle <- paste(stringr::str_wrap(plottitle,width = 35), sep = '')      
      }
      
      y <- rstandard(model)
      qqnorm(y, main=paste(plottitle),
             font.main = 1.0, col.main = "black",col.lab ="black",pch=1,
             xlab = "Theoretical Quantiles", ylab = "Standardized Residuals",
             plot.it = TRUE, datax = FALSE,col = 'black')
      
      qqline(y, datax = FALSE, distribution = qnorm,
             probs = c(0.25, 0.75), qtype = 7)
      
    }
  })
  
  
  observeEvent(input$mQQLink,{
    urlassumption7 <- a("Normal Probability Plot of Residuals at R Tutorial on r-tutor.com", href="https://www.r-tutor.com/elementary-statistics/simple-linear-regression/normal-probability-plot-residuals")
    urlassumption7A <- a("What Are Standardized Residuals? on Statology.org", href="https://www.statology.org/standardized-residuals/")
    
    showModal(
      modalDialog(
        size = 'm',
        align='center',
        title = "Normality of Residuals",
        align='left',
        HTML(paste('<h6><b>',"Intro:",'</b>',
                   "The next assumption is that the residuals generated by the model follow a normal distribution.",
                   "A QQ plot helps visually to check the compliance to this assumption.","<br>",
                   "Here we use standardized residual of the linear regression model and Theoretical Quantiles.",'<br>',
                   "The data points must fall along a rough straight line of 45 degree angles, for the residuals to be normally distributed.",'<br>',
                   '<h6><b>',"Interpretation:",'</b>', "Residuals away from the plotted line are not normally distributed.",
                   "In practice, we often consider any standardized residual with an absolute value greater than 3 to be an outlier.",
                   "Those outliers are to be investigated whether data entry error or some other source")
        ),
        footer = HTML(paste('<b><CENTER>',"Reference 1:",urlassumption7,'<br>',
                            "Reference 2:",urlassumption7A,'</CENTER></b><h6>')),
        easyClose = TRUE)
    )
    
  })
  
  
  
  
  ######################################################
  #Testing of assumptions with Statistics
  ######################################################  
  output$mnormstatisticsTbl <- DT::renderDataTable({
    tt <- names(vmy$normalstatdf)[-1]
    for (c in tt){
      for (nn in 1:nrow(vmy$normalstatdf)){
        if (!is.na(vmy$normalstatdf[nn,c])==TRUE){
          vmy$normalstatdf[nn,c]<- format(round(as.numeric(vmy$normalstatdf[nn,c]), 3), nsmall = 3)
        }
      }
    }
    DT::datatable(vmy$normalstatdf,
                  #class ='cell-border stripe compact white-space: nowrap', #::I got from this site; Thank you this multiple classes: https://rstudio.github.io/DT/
                  escape=F,
                  editable = F,
                  rownames = T,
                  selection = list(mode = "single", target = 'row'),
                  options = list(scrollY = '325px',
                                 dom = 't',
                                 ordering=F,
                                 pageLength = -1,
                                 class="compact",
                                 autoWidth = FALSE,
                                 columnDefs = list(list(width ='3px', targets = c(0)),
                                                   list(width ='125px', targets = c(1)),
                                                   list(width ='10px',targets = c(2)),
                                                   list(width ='10px', targets = c(3)),
                                                   list(width ='10px',targets = c(4)),
                                                   list(width ='10px',targets = c(5)),
                                                   list(width ='10px',targets = c(6)),
                                                   list(className = 'dt-center', targets = 2:6)
                                 )
                  )
                  
    ) %>%
      formatStyle(c(names(vmy$normalstatdf)[2:6]),backgroundPosition = 'center',
                  color = styleInterval(0.05, c('red','green')),
                  backgroundColor = styleInterval(0.05, c('#ffffed', '#ffffed'))
      )%>%
      formatStyle(c(names(vmy$normalstatdf)[2:6]), target= 'row',color = 'black',
                  backgroundColor = '#ffffed',fontSize = '100%',
                  fontWeight ='bold',lineHeight='95%')
  })
  
  
  
  fnNorDisStatisticdf <- function(){
    vmy$normalstatdf <- data.frame(
      Model_      = as.character(),
      Shapiro_W   = as.numeric(),
      Jarque_B    = as.numeric(),
      Pearson_C   = as.numeric(),
      Anderson_D  = as.numeric(),
      Shapiro_F   = as.numeric()
      
    )
    
    n <- nrow(vmy$mModelsdf)
    
    for(i in 1:n) {
      f2 <- as.formula(paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i]))
      model     <- lm(f2, data = vmy$mydata)
      mModelName   <- paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i])
      
      y<- model$residuals
      
      if (nrow(vmy$mydata)<5000){
        mshapiroW <- shapiro.test(y)
      }
      mjarque <- jarque.bera.test(y)
      mpearson     <- pearson.test(y)
      manderson     <- ad.test(y)
      if (nrow(vmy$mydata)<5000){
        mshapiroF    <- sf.test(y)
      }
      
      
      
      vmy$normalstatdf <- vmy$normalstatdf%>%add_row(
        Model_ = mModelName ,
        Shapiro_W  = if (nrow(vmy$mydata)<5000){
          round(mshapiroW$p.value,3)
        },
        Jarque_B      =round(mjarque$p.value,3),
        Pearson_C      = round(mpearson$p.value,3),
        Anderson_D       = round(manderson$p.value,3),
        Shapiro_F       = if (nrow(vmy$mydata)<5000){
          round(mshapiroF$p.value,3)
        }
      )
    }
    
  }
  
  
  fnShowFlashCard <- function(){
    vmy$mFlashCardsDf <- data.frame(front="text",back = "text")[-1,]
    front1 <- "Statistical Test of Normality"
    front2 <-  paste('<h6><b><LEFT>',"Intro:",'</b><br>',"To test the presense of normality, visual observation of plots may not be sufficient.",
                     "The best approach is to combine visual observations and statistical tests.",'<br>','<br>',
                     "The p-value of test statistics of the following tests are reported in the table",'<br>',
                     paste('<h6><b>',"List of tests reported in the table:",'</b><br>',
                           "Sapiro-Wilk test",'<br>',
                           "Jarque-Bera Test",'<br>',  
                           "Pearson chi-square Test; and",'<br>',
                           "Anderson-Darling Test",'<br>',
                           "Shapiro-Francia Test",'<br>'))
    front3 <- paste('<h6><b>',"Reference:",'<br>',urlRegn8,'<br>','<br>',"Hover to see the back side of the card",'<br>',"Most interesting option",'</b><h5>')
    
    back1 <- paste('<h6><b><LEFT>',"Hypothesis:",'</b><br>', "Ho-The null hypothesis is that the sample distribution is normal and",'<br>',"HA-Alternative is NOT Normal",'<br>','<br>')
    back2 <- paste('<h6><b><LEFT>',"Interpretation:",'</b><br>', "if the p-value is more than alpha 0.05, we fail to reject NULL hypothesis, hence NORMALITY exists;
               But if p-value is less than 0.05, the distribution is NOT Normal",'<br>',
                   "In the table reported on the left, the p-values in 'Green' confirms the presence of normality; figures in RED shows NOT Normal",'<br>','<br>')
    back3 <- paste('<h6><b>',"Reference:",'<br>',urlRegn8,'</b><h5>')
    
    vmy$mFlashCardsDf <- vmy$mFlashCardsDf  %>% add_row(
      
      front = c(front1,front2,front3 ),
      back = c(back1,back2,back3)
    )
    
    output$mflashcardUI <- renderUI({
      align="left"
      flashCardOutput("card1", width = '100%',height = '400px')
      
    })
    
    
    output$card1 <- renderFlashCard({
      flashCard(
        vmy$mFlashCardsDf,
        frontColor = "white",
        backColor = "#ffffcd",
        front_text_color = "black",
        back_text_color = "black",
        elementId = NULL
      )
    })
  }
  
  
  
  ######################################################
  #Testing Assumption:  Mesure of Influence
  ###################################################### 
  output$mMeasureofIncluencePlot <-renderPlot({
    theme_set(
      theme_grey(12)
    )
    
    n <- nrow(vmy$mModelsdf)
    c <- ifelse (n>2,ceiling(n/2),n)
    r <- ifelse (n<= 2,1,2)
    
    par(mfrow=c(r,c)) #get in 3 rows and 3 columns (first one is row)
    my_plots <- list()
    mformulatxt <- ""
    
    library(gridExtra)
    for(i in 1:n) {
      f2 <- as.formula(paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i]))
      model <- lm(f2, data = vmy$mydata)
      mformulatxt <- paste(paste( text=vmy$mModelsdf$Dependent_[i],"~"), vmy$mModelsdf$Predictor_[i])
      
      my_plots[[i]] <- fnGenerateMeasureofInflu(model,mformulatxt)
      
    }
    grid.arrange(grobs =my_plots,nrow=r, ncol=c)
  })
  
  
  fnGenerateMeasureofInflu <- function(model,mformulatxt){
    mfontsize <- ifelse(nrow(vmy$mModelsdf)>4,10,12)
    #ols_plot_cooksd_bar
    #ols_plot_resid_stand
    p<-""
    plottitle <-""
    
    if (input$mChangeChart%%2==1){
      plottitle <- paste("Stanzd.Resil.::",paste0("(",mformulatxt,")") )
      p <- ols_plot_resid_stand(model)
    }
    else{
      plottitle <- paste("Cook's D Bar::",paste0("(",mformulatxt,")") )
      p <- ols_plot_cooksd_bar(model)
    }
    
    if (nrow(vmy$mModelsdf)>6){
      plottitle <- paste(stringr::str_wrap(plottitle,width = 35), sep = '')      
    }
    
    p +
      ggtitle(plottitle)+              #title code where you got: https://github.com/rsquaredacademy/olsrr/blob/master/R/ols-cooks-d-barplot.R
      theme_gray(base_size = mfontsize)+
      theme(
        legend.position  = 'none',
        text = element_text(color = "black", size = mfontsize,face = "italic"),complete = TRUE
        #panel.background = element_rect(fill='#363636'), plot.background = element_rect(fill='#363636')
      )
  }
  
  output$mMItabtitle <- renderUI({
    if (input$mChangeChart%%2==1){
      HTML(paste('<h5><b><CENTER>',"Test-Detect Influential Observations with Standardized Residual Chart",'</CENTER></b>'))
    }
    else {
      HTML(paste('<h5><b><CENTER>',"Test-Detect Influential Observations with Cook's D Bar (Red bars are Influencial observations)",'</CENTER></b>'))
      
    }
  })
  
  observeEvent(input$mMILink,{
    urlassumption8 <- a("olsrr documentation-Measures of Influence in Linear Regression model", href="https://olsrr.rsquaredacademy.com/articles/influence_measures.html#cooks-d-bar-plot")
    urlassumption8A <- a("Modification and formating of title-github.com on olsrr package", href="https://github.com/rsquaredacademy/olsrr/blob/master/R/ols-cooks-d-barplot.R")
    
    showModal(
      modalDialog(
        size = 'm',
        align='center',
        title = "Measure of Influence",
        align='left',
        HTML(paste('<h6><b>',"Intro:",'</b>',
                   "It is possible for a single observation to have a great influence on the results of a regression analysis. Cook's distance, Di, is used in regression analysis to identify such influential data points that may negatively affect your regression model. ",'<br>','<br>',
                   "Cook's Distance measure, first deletes one observation, refits the regression model with the rest of the observations, examines how much all the fitted values change when that one observation is",
                   "deleted. A data point having a large cook's D indicates that the data point strongly influences the fitted values.","<br>",
                   "In the Cook's D chart, observations in blue are normal and outliers marked in red" ,"<br>",
                   vmy$MOITxt )
        ),
        footer = HTML(paste('<b><CENTER>',"Reference 1:",urlassumption8,'<br>',
                            "Reference 2:",urlassumption8A,'</CENTER></b><h6>')),
        easyClose = TRUE)
    )
    
  })
  
  ######################################################
  #Testing of assumptions ALL in One
  ###################################################### 
  output$mAllInOnePlot <-renderPlot({
    theme_set(
      theme_grey(12)
    )
    
    par(mfrow=c(2,3)) #get in 2 rows and 3 columns (first one is row)
    uuvv <- vmy$tempdf2 %>%dplyr::filter(vmy$tempdf2$Predictor_ %in% input$mSelectedModelAllInOneTab)
    f2 <- as.formula(paste(paste( text=uuvv[1,1],"~"), uuvv[1,2]))
    
    model <- lm(f2, data = vmy$mydata)
    output$mALLInOneTxt <- renderUI({
      tryCatch({
        HTML(paste('<h5><b>','Model:',paste(paste( text=uuvv[1,1],"~"), uuvv[1,2]),'</b>'))
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    })
    
    
    class(autoplot(model))
    autoplot(model, which = 1:6, ncol = 3) +
      theme(
        legend.position  = 'none',
        text = element_text(color = "black", size = 11,face = "italic"),complete = TRUE
        #panel.background = element_rect(fill='#363636'), plot.background = element_rect(fill='#363636')
      )
    
  })
  
  # 
  
  
} #server closure
shinyApp(ui, server)