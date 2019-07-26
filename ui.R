#Ariel Macari
# ST558 Final Project
# Objective: Build a shiny app - this is the ui portion


#packages required for this work
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(caret)
library(gridExtra)
library(arsenal)
library(plotly)
library(party)
library(e1071)



#Data is brought in as an RDS to save computing.
 data1<- readRDS(data, file = "./data.rds")
 data2<- data1

#The following code is to display the dashboard title 
 header<-  dashboardHeader(title = "Ariel Macari Final Project ST558, Summer 2019")

#The following code creates the sidebar and menu
  #this is for the working tabs item!
 sidebar<-   dashboardSidebar(
  sidebarMenu(
    menuItem("Info Page", tabName = "first", icon = icon("dashboard")),
    menuItem("Data Exploration", tabName = "second", icon = icon("th")),
    menuItem("Unsupervised Learning", tabName = "third", icon = icon("bar-chart-o")),
    menuItem("Supervised Learning", tabName = "fourth", icon = icon("bar-chart-o")),
    menuItem("Project Data", tabName = "fifth", icon = icon("table"))
  )
)

#The following creates the dashboard body
 body<- dashboardBody(
  tabItems(
    
#includes the first tab including a description of the project, including hyperlinks
    #this is the information page item!
    tabItem(tabName = "first",
            h1("Information Page"),
            h4("The objective of this R Shiny Application is to build models that relate   molecular information to an actual biological response.  This data was originally from a competition held by Boehringer Ingelheim through Kaggle."),
            br(),
            h4("Each row represents a molecule. There are 1776 columns (variables d1 through d1776) representing a                      molecule characteristic for example size, shape, or elemental constitution, which have been normalized.The last column represents if the molecule elicited a response (1) or not (0)."),
            br(),
            h4("The Application is broken down into five (5) tabs.  This first tab describes the project.  The second tab allows for numeric and graphical summaries.  The third tab is for Unsupervised Learning, which is for learning in patterns and relationships in the data (No output to shoot for).  The fourth tab in this App is for Supervised Learning, which here is used to build a model and predict on an elicited response. The last tab in this App is showing the data used in this study in a datatable and also allows for the data to be downloaded by the end user."),
            br(),
            h4("On the unsupervised learning tab - Principal Components analysis (PCA) was used.  PCA is a dimension reduction technique.  PCA looks for linear combinations of those variabiles that account for most of the variability.  On the unsupervised learning tab, a biplot is shown. "),
            br(),
            h4("On the supervised learning tab - there are two possible options to model the data: The first option is a classification Tree.  In tree based methods, the predictor space is spilt up into different regions with different predictions for each region.  For each split, the Gini Index (shown below where p = P(correct classification)) is minimized. For each region, the most prevelent class is used as the predictor."),
            br(),
            uiOutput('ex1'),
            br(),
           h4("The other option on the supervised learning tab is treebaging.  In tree bagging, a bootstrapped sample (same size as oroginal sample) is created by sampling with replacement from the samples.  A tree is trained on the sample.  This process is repeated many times. "),
           br(),
           h4("The advantage of treebagging is that prediction improves compared with the tree approach. The disadvantage of treebagging is that the interpretation of the tree is lost."),
           br(),
            h4("Some Relevent Links are shown below "),
            br(),
            tags$a(href="https://datahub.io/machine-learning/bioresponse", 
                   "Link to where The data was obtained"),
            br(),
            tags$a(href="https://www.boehringer-ingelheim.com/", 
                   "Link to Boehringer Ingelheim"),
            br(),
            tags$a(href="https://www.kaggle.com/c/bioresponse", 
                   "Link to Kaggle Site with the original competition")
            ),
    
#The following creates the second tab including a header, the first uioutput that creates a dynamic column, selecting a column, a plot (has 2 in there), and a hovering plot    
    #This creats the code for the data exploration  item.  Althugh the assignment only requests to save one graph - both grahs are savable
    tabItem(tabName = "second",
            h1("Data Exploration Page"),
            br(),
            uiOutput("title"), 
            br(),
            selectInput("characteristic", "Choose a characteristic to compare against the target (Type D1 up to D1776):", 
                  choices = names(data1[,-1777]), selected = names(data1)[[2]]),
            h2("The following plot shows a histogram (stacked) and the empirical distribution"),
            # naming convention - second tab, first plot, same throught the document
            plotOutput("Plot21"),
            downloadButton('downloadPlot21', 'Download this Plot',style="display: block; margin: 0 auto; width: 230px;color: black;"), #downloadbutton
            h2("The following table helps determine if there is a statistical difference in the variable"), 
            tableOutput("table21"),
            h2("The following plot shows a side by side comparison of the two distributions"),
            h6("this side by side graph has a button to allow the plot to be downloaded.  The plot also allows the user to click on the plot"),
            h3("0 is target not achieved, 1 is target achieved"),
            #the following satisfies the plot click or hover requirement
            plotlyOutput("Plot22"),
            h6("this side by side graph has a button to allow the plot to be downloaded.  The plot also allows the user to click on the plot"),
            #save data            
            downloadButton('downloadData21', 'Download Raw Data used to make this Plot',style="display: block; margin: 0 auto; width: 230px;color: black;") 
            ),

#The folllowing code creates the third tab which is for the unsupervised learning.  It includes the second uioutput and a biplot.    
    #this creates the unsupervised learning page
    tabItem(tabName = "third",
            h1("Unsupervised Learning"),
            br(),
            uiOutput("title1"),
            br(),
            h4("select desired columns to analyze.  Delete to remove the default."),
            h5("A minimum of four selections are required"),
            selectInput("select3", "Select columns to display", names(data1),selected = c(names(data1)[[27]], names(data1)[[182]],
                 names(data1)[[217]],names(data1)[[469]],names(data1)[[747]],names(data1)[[1777]]), multiple = TRUE),
            selectInput("PC1", "Choose the PC to plot on X axis",choices = list(1,2,3,4), selected= 1), 
            selectInput("PC2", "Choose the PC to plot on Y axis",choices = list(1,2,3,4), selected = 2), 
            h5("The plot below can be saved by clicking on the plot, hitting save, and adding a .png to the end of the file"),
            downloadButton('downloadData31', 'Download Raw Data used in the analysis for this Plot',
                style="display: block; margin: 0 auto; width: 230px;color: black;"),
            imageOutput("Plot31")
            ),

#The following creates the modeling page;
    tabItem(tabName = "fourth",
            h1("Supervised Learning"),
            selectInput("select4", "Select columns to display", names(data1),selected = c(names(data1)[[27]], names(data1)[[182]],
             names(data1)[[217]],names(data1)[[469]],names(data1)[[747]]), multiple = TRUE),
            h4("The tree model creates a decision tree.  The tree bag model does not have a decision tree."),
            br(),
            checkboxInput("tree", value = TRUE, h4("Build a tree model?", style = "color:black;")),
            conditionalPanel("input.tree",  
                  selectInput("maxdepth", "Set the maximum depth of the tree",
                        c(1,2,3,4,5,6,7,8,9,10,11, 12, 13, 14, 15, 16, 17, 18, 19, 20), selected=1),
                  plotOutput("Plot41")),
            tableOutput("table41"),
            textOutput("info")

    ),

#This tab allows the user to see and download the data  
  #Data page with subsetting
    tabItem(tabName = "fifth",
            h1("Project Data"),
            br(),
            h4("The data can be subsetted here.  The Target is the response. The download button downloads the entire dataset."),
            br(),
            selectInput("select5", "Select columns to display", names(data1), multiple = TRUE),
            # Download Button
            downloadButton("downloadData51", "Download Data"),
            mainPanel(dataTableOutput("table51"))
           )
  )
)

#This runs the ui portion of rshiny and sets a black skin.

ui <- dashboardPage(header, sidebar, body, skin="black")
  






