#Ariel Macari
# ST558 Final Project
# Objective: Build a shiny app- this is the server portion


shinyServer(function(input, output, session) {
 
#Data is brought in as an RDS to save computing.
  data1<- readRDS(data, file = "./data.rds")

#__________________Tab 1_______________________________

  #The following is what builds the first tab of the dashboard 
 
  
  #this mathjax statement creates the formula 
  output$ex1 <- renderUI({
    withMathJax(helpText('Gini Index:  $$2p(1-p) $$'))
  })  
  

  
#__________________Tab 2_______________________________
#The following is what builds the second tab of the dashboard 

#the select the columns desired plot
  subset_me <- reactive({
    newData <- data1 %>% select(c(input$characteristic,target))
  })  
    

  
#This code is for the first Dynamic UI element;
  output$title <- renderUI({
    h2("A plot of characteristic", input$characteristic, "versus the target is included on this page as a histogram and ECDF.  There is also a side by side histogram that allows the user to click on the bin.", sep = " ")
  })  
  
  
#This code allows the user to plot the data with a histogram and a emperical distribution plot.
  
  plotInput<-   reactive({ 
      data1<-subset_me()
      p1<- ggplot(data1,aes(x=data1[,1])) + geom_histogram(aes(y=..density.., fill=as.factor(target)), bins=50) + scale_fill_discrete(name = "Target  Achieved?", labels = c("No", "Yes")) + theme(text=element_text(size=14), axis.title.x = element_blank())
      p2<- ggplot(data1,aes(x=data1[,1])) + stat_ecdf(geom="step", aes(color=as.factor(target))) + ylab("ECDF")  + theme(legend.position="none",       
         axis.title.x = element_blank(), text=element_text(size=14))
      grid.arrange(p1,p2, ncol=2)
      })
  
  output$Plot21<-   renderPlot({   
       print(plotInput())
        })
  
  #This code gives a side by side plot and allows the user to click on the plot. 
   
  output$Plot22<-   renderPlotly({ 
       data1<-subset_me()
      p1<- ggplot(data1,aes(x=data1[,1])) + geom_histogram(aes(y=..density..), bins=50) +facet_wrap(.~target) + theme(text=element_text(size=14), 
          axis.title.x = element_blank())
      plotly_build(p1)
             
      })
  
#This code allows the user to have a summary table
  output$table21<- renderTable({
      data1<-subset_me()
      summary(tableby(target~ ., data = data1), text=TRUE)
        })
    
  # Downloadable csv of selected data 
  output$downloadData21 <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv( subset_me(), file)
    })  
  
    #this code allows the user to download the first plot  
  
    output$downloadPlot21 <- downloadHandler(
    filename = function() { paste("dataset-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(file,plotInput(), width = 14, height = 8 )
    })
    
#__________________Tab 3_______________________________
    #The following is what builds the third tab of the dashboard 

    #subsetting the data
    subset_me1 <- reactive({
      newData <- data1 %>% select(c(input$select3))
    })

    
#This code is for the second Dynamic UI element
    output$title1 <- renderUI({
      h2("A biplot of PC",  input$PC1, "VS", input$PC2, "are shown below.", sep = " ")
    })      
    
#This code creates the plots    
    plotInput31<-   reactive({ 
        data1<-subset_me1()
        PC1<- as.numeric(input$PC1)
        PC2<- as.numeric(input$PC2)
        PCs <- prcomp(subset_me1() , center = TRUE, scale = TRUE)
        biplot(PCs, choices = c(PC1,PC2))
          })

#This plot renders the plot and allows the user to download the plot    
    output$Plot31<-   renderImage({ 
      outfile <- tempfile(fileext = '.png')
      png(outfile, width = 800, height = 500)
      print(plotInput31())
      dev.off()
      # Return a list containing the filename
      list(src. = outfile,
           contentType = 'image/png',
           width = 800,
           height = 500)
    })
    
    
 # Downloadable csv of selected data 
    output$downloadData31 <- downloadHandler(
      filename = function() { 
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv( subset_me1(), file)
      })      

    
    
     
#__________________Tab 4_______________________________
    #The following is what builds the third tab of the dashboard     

     
    # the following recodes target as a factor
    data2 <- data1
    data2$target <-as.factor(data2$target)
    
    #create a training and a testing set of data
    set.seed(1)  
    train <- sample(1:nrow(data2), size = nrow(data2)*.8)
    test <- dplyr::setdiff(1:nrow(data2), train)
    
    # The training and test sets rows
    Data_Train <- data2[train, ]
    Data_Test <- data2[test, ]
    
    
    #the following selects the columns for the training set
    train_set1 <- reactive({
      newData <- Data_Train %>% select(c(input$select4),target)
    })
 
    #the following selects the columns for the testing set
    test_set1 <- reactive({
      newData <- Data_Train %>% select(c(input$select4),target)
    })   

    #The following creates the tree model or tree bagging model (depends on what the user check.)
     Tree_train1<- reactive({
       train_set<-train_set1()
       set.seed(1)    
       
       if (input$tree) {
       train(target ~ ., data=train_set, method="ctree2",
         trControl=trainControl(method="repeatedcv", number =  10, repeats = 3, savePredictions = TRUE),
         preProcess= c("center","scale"),
         tuneGrid= expand.grid(mincriterion = .95, maxdepth = as.integer(input$maxdepth)))}
       
       else{
       train(target ~ ., data=train_set, method = "treebag",
                      trControl =trainControl(method="repeatedcv", number =  10, repeats = 3),
                      preProcess= c("center","scale"))
      } 
         })

     #The following predicts using the  model
    
     predTree1 <- reactive({
      test_set<-test_set1() 
       Train<- Tree_train1()
       predict(Train,test_set)
     })

     
     #The following creates the confusion matrix      
     output$table41<- renderTable({
       test_set<- test_set1()
       predTree<-predTree1()
       res<-confusionMatrix(test_set$target, predTree)
       res$table
     })
    
     #create a statement on the prediction of the model info
     output$info <- renderText({
       #get data
       test_set<- test_set1()
       predTree<-predTree1()
       res<-confusionMatrix(test_set$target, predTree)
       
       #paste info out
       paste("The accuracy in your chosen model is ",  (round(res$overall["Accuracy"],3)*100), "%.", sep = " ")
   })    

     
     #create the tree plot
     output$Plot41<-   renderPlot({   
       trained<-Tree_train1()
       print_me<- trained$finalModel
       
       if (input$tree) {
       
       print(plot(print_me))}
       
       else{NULL}

     })
 
 
#__________________Tab 5_______________________________  
#The folllowing code is for seeing and downloading the data in the fifth tab  
     
     #subsetting the data for tab 5
     subset_me5 <- reactive({
       newData <- data1 %>% select(c(input$select5))
     })     
     
  
#This code renders the table and allows the user to select the columns, fifth tab first table
  output$table51 =  DT::renderDataTable(
       subset_me5(),
       options = list(scrollX = TRUE))

 
  
# Downloadable csv of  dataset, fifth tab, first download button
  output$downloadData51 <- downloadHandler(
    filename = function() { 
      paste("dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(subset_me5(), file)
    })

   
}
)
