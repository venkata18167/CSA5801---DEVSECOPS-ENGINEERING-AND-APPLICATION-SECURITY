library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)

stages <- c("Code Commit","Static Analysis","Dependency Scan","Unit Testing","Build","Security Testing","Deployment")
scenarios <- c("Balanced Pipeline","Security Intensive","High Load Pipeline","Fast Delivery")

make_data <- function(n=1000) {
  data.frame(
    Simulation_ID=1:n,
    Code_Changes=sample(5:80,n,TRUE),
    Dependencies=sample(10:180,n,TRUE),
    Vulnerabilities=rpois(n,3),
    Test_Coverage=round(pmin(100,pmax(45,rnorm(n,82,10))),2),
    Build_Time=round(rlnorm(n,log(4),.35),2),
    Security_Scans=sample(1:8,n,TRUE),
    Deployment_Frequency=sample(1:15,n,TRUE),
    Failure_Rate=round(pmin(25,pmax(.1,rnorm(n,5,3))),2),
    Recovery_Time=round(pmax(1,rlnorm(n,log(12),.45)),2)
  )
}

simulate <- function(s) {
  m <- switch(s,
              "Balanced Pipeline"=c(1,1,1,1,1,1,1),
              "Security Intensive"=c(1.1,1.4,1.5,1.1,1.1,1.8,1.05),
              "High Load Pipeline"=c(1.4,1.25,1.3,1.45,1.5,1.35,1.25),
              "Fast Delivery"=c(.75,.85,.8,.8,.75,.9,.8))
  base <- c(2,3.2,2.5,4,3,5,2)
  mins <- round(pmax(.5,base*m*runif(7,.78,1.28)),2)
  score <- round(pmin(100,pmax(0,100-sum(mins)*.7-rnorm(1,0,4)-sample(0:18,1))),1)
  list(
    stages=data.frame(Stage=stages,Minutes=mins,
                      Status=ifelse(runif(7)>.12,"Completed","Warning")),
    time=round(sum(mins),2), score=score,
    vuln=sample(0:12,1), failures=sample(0:8,1),
    coverage=round(runif(1,68,98),1),
    risk=ifelse(score>=85,"Low",ifelse(score>=65,"Medium","High")),
    stamp=format(Sys.time(),"%Y-%m-%d %H:%M:%S"))
}

ui <- dashboardPage(
  dashboardHeader(title="ShiftLeftX | Secure Pipeline Simulator"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard",tabName="dash",icon=icon("dashboard")),
    menuItem("Live Simulation",tabName="sim",icon=icon("play-circle")),
    menuItem("Analytics",tabName="analytics",icon=icon("line-chart")),
    menuItem("Dataset",tabName="data",icon=icon("table")),
    menuItem("Scenarios",tabName="scenarios",icon=icon("random")),
    menuItem("About",tabName="about",icon=icon("info-circle"))
  )),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper,.right-side{background:#f4f6f9}
      .small-box,.box{border-radius:12px}
      .simcard{background:linear-gradient(135deg,#0f2747,#173f6b);
      color:white;padding:25px;border-radius:15px;margin-bottom:20px}
    "))),
    tabItems(
      tabItem("dash",
              fluidRow(valueBoxOutput("runs",3),valueBoxOutput("time",3),
                       valueBoxOutput("security",3),valueBoxOutput("risk",3)),
              fluidRow(
                box(title="Simulation Performance",status="primary",solidHeader=TRUE,width=8,
                    plotlyOutput("history_plot",height=350)),
                box(title="Security Distribution",status="info",solidHeader=TRUE,width=4,
                    plotlyOutput("security_plot",height=350))),
              fluidRow(box(title="Latest Runs",status="success",solidHeader=TRUE,width=12,
                           DTOutput("history_table")))),
      
      tabItem("sim",
              div(class="simcard",
                  h2("Secure Software Delivery Simulation"),
                  p("Every run generates new random execution, security and risk values."),
                  selectInput("scenario","Simulation Scenario",scenarios),
                  actionButton("run","RUN NEW SIMULATION",icon=icon("play"),
                               class="btn-primary btn-lg")),
              fluidRow(valueBoxOutput("simtime",3),valueBoxOutput("simsec",3),
                       valueBoxOutput("simvuln",2),valueBoxOutput("simfail",2),
                       valueBoxOutput("simcov",2)),
              fluidRow(
                box(title="Pipeline Stage Execution",status="primary",solidHeader=TRUE,width=8,
                    plotlyOutput("stage",height=400)),
                box(title="Security Score",status="warning",solidHeader=TRUE,width=4,
                    plotlyOutput("gauge",height=400))),
              fluidRow(box(title="Current Simulation",status="info",solidHeader=TRUE,width=12,
                           DTOutput("simtable")))),
      
      tabItem("analytics",
              fluidRow(
                box(title="Coverage vs Failure Rate",status="primary",solidHeader=TRUE,width=6,
                    plotlyOutput("scatter",height=350)),
                box(title="Vulnerability Distribution",status="danger",solidHeader=TRUE,width=6,
                    plotlyOutput("vulnplot",height=350))),
              fluidRow(
                box(title="Build Time Distribution",status="warning",solidHeader=TRUE,width=6,
                    plotlyOutput("buildplot",height=350)),
                box(title="Security Scan Frequency",status="success",solidHeader=TRUE,width=6,
                    plotlyOutput("scanplot",height=350)))),
      
      tabItem("data",
              box(title="1000-Record Generated Dataset",status="primary",solidHeader=TRUE,width=12,
                  downloadButton("download","Download CSV"),br(),br(),DTOutput("datatable"))),
      
      tabItem("scenarios",
              box(title="Simulation Scenarios",status="primary",solidHeader=TRUE,width=12,
                  DTOutput("scenariotable")),
              box(title="Scenario Comparison",status="info",solidHeader=TRUE,width=12,
                  plotlyOutput("scenariochart",height=400))),
      
      tabItem("about",
              box(title="ShiftLeftX",status="primary",solidHeader=TRUE,width=12,
                  h3("Discrete Event Modeling of Secure Software Delivery Pipelines"),
                  p("Interactive Shiny dashboard for secure CI/CD simulation and analysis."),
                  tags$ul(
                    tags$li("1000-record synthetic dataset"),
                    tags$li("Four simulation scenarios"),
                    tags$li("Different random result on every run"),
                    tags$li("Security score, risk and vulnerability analysis"),
                    tags$li("Interactive Plotly charts"),
                    tags$li("CSV download"))))
    )
  )
)

server <- function(input,output,session){
  dataset <- reactiveVal(make_data())
  current <- reactiveVal(NULL)
  hist <- reactiveVal(data.frame(Run=integer(),Scenario=character(),
                                 Total_Time=numeric(),Security_Score=numeric(),Vulnerabilities=integer(),
                                 Failures=integer(),Coverage=numeric(),Risk=character(),Timestamp=character()))
  
  observeEvent(input$run,{
    x <- simulate(input$scenario); current(x)
    h <- hist()
    hist(rbind(h,data.frame(Run=nrow(h)+1,Scenario=input$scenario,
                            Total_Time=x$time,Security_Score=x$score,Vulnerabilities=x$vuln,
                            Failures=x$failures,Coverage=x$coverage,Risk=x$risk,Timestamp=x$stamp)))
  })
  
  output$runs<-renderValueBox(valueBox(nrow(hist()),"Simulation Runs",icon=icon("play-circle"),color="aqua"))
  output$time<-renderValueBox({h<-hist();valueBox(if(!nrow(h))"—"else paste0(round(mean(h$Total_Time),2)," min"),"Average Pipeline Time",icon=icon("clock-o"),color="yellow")})
  output$security<-renderValueBox({h<-hist();valueBox(if(!nrow(h))"—"else paste0(round(mean(h$Security_Score),1),"%"),"Average Security",icon=icon("shield"),color="green")})
  output$risk<-renderValueBox({h<-hist();valueBox(if(!nrow(h))"Not Run"else h$Risk[nrow(h)],"Latest Risk",icon=icon("warning"),color="blue")})
  
  output$simtime<-renderValueBox({x<-current();valueBox(if(is.null(x))"—"else paste0(x$time," min"),"Execution Time",icon=icon("clock-o"),color="aqua")})
  output$simsec<-renderValueBox({x<-current();valueBox(if(is.null(x))"—"else paste0(x$score,"%"),"Security Score",icon=icon("shield"),color="green")})
  output$simvuln<-renderValueBox({x<-current();valueBox(if(is.null(x))"—"else x$vuln,"Vulnerabilities",icon=icon("bug"),color="red")})
  output$simfail<-renderValueBox({x<-current();valueBox(if(is.null(x))"—"else x$failures,"Failures",icon=icon("warning"),color="yellow")})
  output$simcov<-renderValueBox({x<-current();valueBox(if(is.null(x))"—"else paste0(x$coverage,"%"),"Test Coverage",icon=icon("check"),color="green")})
  
  output$stage<-renderPlotly({x<-current();req(x);p<-ggplot(x$stages,aes(Stage,Minutes,text=paste(Stage,Minutes,Status,sep="<br>")))+geom_col()+coord_flip()+theme_minimal()+labs(x=NULL,y="Minutes");ggplotly(p,tooltip="text")})
  output$gauge<-renderPlotly({x<-current();req(x);plot_ly(type="indicator",mode="gauge+number",value=x$score,title=list(text=paste("Risk:",x$risk)),gauge=list(axis=list(range=list(0,100))))})
  output$simtable<-renderDT({x<-current();req(x);datatable(x$stages,rownames=FALSE,options=list(dom="t"))})
  
  output$history_plot<-renderPlotly({h<-hist();req(nrow(h)>0);plot_ly(h,x=~Run,y=~Total_Time,color=~Scenario,type="scatter",mode="lines+markers")%>%layout(xaxis=list(title="Run"),yaxis=list(title="Minutes"))})
  output$security_plot<-renderPlotly({h<-hist();req(nrow(h)>0);plot_ly(h,x=~Security_Score,type="histogram")})
  output$history_table<-renderDT({datatable(tail(hist(),10),rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))})
  
  output$scatter<-renderPlotly({d<-dataset();plot_ly(d,x=~Test_Coverage,y=~Failure_Rate,type="scatter",mode="markers",text=~paste("Coverage:",Test_Coverage,"<br>Failure:",Failure_Rate))})
  output$vulnplot<-renderPlotly({plot_ly(dataset(),x=~Vulnerabilities,type="histogram")})
  output$buildplot<-renderPlotly({plot_ly(dataset(),x=~Build_Time,type="histogram")})
  output$scanplot<-renderPlotly({d<-dataset()%>%count(Security_Scans);plot_ly(d,x=~Security_Scans,y=~n,type="bar")})
  output$datatable<-renderDT({datatable(dataset(),rownames=FALSE,filter="top",options=list(pageLength=15,scrollX=TRUE))})
  output$download<-downloadHandler(
    filename=function()paste0("ShiftLeftX_dataset_",Sys.Date(),".csv"),
    content=function(file)write.csv(dataset(),file,row.names=FALSE))
  output$scenariotable<-renderDT({datatable(data.frame(
    Scenario=scenarios,
    Description=c("Normal balanced pipeline","Extra security testing",
                  "Heavy load and longer execution","Speed optimized pipeline")),
    rownames=FALSE,options=list(dom="t"))})
  output$scenariochart<-renderPlotly({
    h<-hist();req(nrow(h)>0)
    s<-h%>%group_by(Scenario)%>%summarise(Avg_Time=mean(Total_Time),.groups="drop")
    plot_ly(s,x=~Scenario,y=~Avg_Time,type="bar")%>%layout(yaxis=list(title="Average Time (min)"))
  })
}
shinyApp(ui,server)