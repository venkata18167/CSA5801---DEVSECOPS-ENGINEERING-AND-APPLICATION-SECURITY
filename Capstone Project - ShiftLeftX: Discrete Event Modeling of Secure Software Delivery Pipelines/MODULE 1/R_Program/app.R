
library(shiny)
library(shinydashboard)
library(DT)

data <- read.csv("pipeline_data.csv")

ui <- dashboardPage(
 dashboardHeader(title="ShiftLeftX"),
 dashboardSidebar(
   sidebarMenu(
     menuItem("Dashboard", tabName="dash", icon = icon("dashboard")),
     menuItem("Dataset", tabName="data", icon = icon("table")),
     menuItem("Visualization", tabName="viz", icon = icon("chart-bar")),
     menuItem("Statistics", tabName="stats", icon = icon("calculator")),
     menuItem("Pipeline Simulation", tabName="simulation", icon = icon("play"))
   )
 ),
 dashboardBody(
  tabItems(
   tabItem(tabName="dash",
     fluidRow(
       valueBoxOutput("records"),
       valueBoxOutput("completed"),
       valueBoxOutput("failed")
     )
   ),
   tabItem(tabName="data",
     DTOutput("dataset")
   ),
   tabItem(tabName="viz",
     selectInput("graph","Graph Type",
                 c("Bar","Pie","Scatter","Histogram","Line","Box")),
     plotOutput("plot",height="450px")
   ),
   tabItem(tabName="stats",
     tableOutput("statsTable"),
     downloadButton("download","Download CSV")
   ),
   tabItem(
     tabName = "simulation",
     
     fluidRow(
       
       box(
         width = 4,
         title = "Pipeline Settings",
         status = "primary",
         solidHeader = TRUE,
         
         textInput("developer", "Developer Name", "Archana"),
         
         selectInput(
           "project",
           "Select Project",
           choices = c(
             "Banking System",
             "Hospital Management",
             "College Management",
             "E-Commerce Website"
           )
         ),
         
         actionButton("start", "Start Pipeline", icon = icon("play"))
       ),
       
       box(
         width = 8,
         title = "Pipeline Execution",
         status = "success",
         solidHeader = TRUE,
         
         tableOutput("pipeline"),
         
         br(),
         
         h4("Total Execution Time"),
         textOutput("time"),
         
         br(),
         
         h4("Security Score"),
         textOutput("score"),
         
         br(),
         
         plotOutput("graph", height = "350px")
       )
       
     )
   )
  )
 )
)

server <- function(input, output){

 output$records <- renderValueBox(
   valueBox(nrow(data),"Total Records",icon=icon("database"),color="blue"))

 output$completed <- renderValueBox(
   valueBox(sum(data$Status=="Completed"),"Completed",icon=icon("check"),color="green"))

 output$failed <- renderValueBox(
   valueBox(sum(data$Status=="Failed"),"Failed",icon=icon("times"),color="red"))

 output$dataset <- renderDT(datatable(data,options=list(pageLength=10,scrollX=TRUE)))

 output$statsTable <- renderTable({
   data.frame(
     Total_Records=nrow(data),
     Completed=sum(data$Status=="Completed"),
     Failed=sum(data$Status=="Failed"),
     Average_Time=round(mean(data$Time),2),
     Total_Vulnerabilities=sum(data$Vulnerabilities)
   )
 })

 output$plot <- renderPlot({
   switch(input$graph,
     "Bar"=barplot(table(data$Stage),las=2,col="steelblue"),
     "Pie"=pie(table(data$Severity)),
     "Scatter"=plot(data$Time,data$Vulnerabilities,pch=19,col="red",
                    xlab="Time",ylab="Vulnerabilities"),
     "Histogram"=hist(data$Time,col="lightblue"),
     "Line"=plot(data$Time,type="l",col="blue"),
     "Box"=boxplot(data$Time,col="orange")
   )
 })

 output$download <- downloadHandler(
   filename=function(){"pipeline_data.csv"},
   content=function(file){file.copy("pipeline_data.csv",file)}
 )
 
 observeEvent(input$start, {
   
   pipeline <- data.frame(
     Stage = c(
       "Code Commit",
       "Static Analysis",
       "Dependency Scan",
       "Unit Testing",
       "Build",
       "Security Testing",
       "Deployment"
     ),
     Status = rep("Completed", 7)
   )
   
   output$pipeline <- renderTable({
     pipeline
   })
   
   output$time <- renderText({
     "21 Minutes"
   })
   
   output$score <- renderText({
     "98% Secure"
   })
   
   output$graph <- renderPlot({
     
     stages <- c(2,3,2,4,3,5,2)
     
     names(stages) <- c(
       "Commit",
       "Static",
       "Dependency",
       "Testing",
       "Build",
       "Security",
       "Deploy"
     )
     
     barplot(
       stages,
       col = "steelblue",
       main = "Pipeline Execution Time",
       ylab = "Minutes"
     )
     
   })
   
 })
 
}   # <-- This closes the server function

shinyApp(ui, server)
