set.seed(123)

n <- 1000

pipeline <- data.frame(
  
  Build_ID = 1:n,
  
  Developer_ID = sample(1:100,n,replace=TRUE),
  
  Commit_Size = sample(20:1000,n,replace=TRUE),
  
  Build_Time = round(rnorm(n,15,4),2),
  
  Test_Time = round(rnorm(n,10,3),2),
  
  Security_Scan_Time = round(rnorm(n,8,2),2),
  
  Vulnerabilities = sample(0:10,n,replace=TRUE),
  
  Deployment_Time = round(rnorm(n,6,1.5),2),
  
  CPU_Usage = round(runif(n,20,95),2),
  
  Memory_Usage = round(runif(n,30,90),2)
  
)

head(pipeline)

write.csv(pipeline,"pipeline_data.csv",row.names=FALSE)