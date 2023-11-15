if(F){config = configr::read.config("config.yml")$default
conn <- dbConnect(
  drv = RMySQL::MySQL(max.con=100, fetch.default.rec=1000),
  dbname = config$sql$dbname,
  host = config$sql$host,
  port = config$sql$port,
  username = config$sql$username,
  password = config$sql$password
)
source("Rfiles-server/_global_functions.R")
}
detachAllPackages <- function() {

  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

  package.list <- setdiff(package.list,basic.packages)

  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

}
if(F){
  detachAllPackages()
  devtools::install_github("stevepowell99/causalmap3functions@main",force=T,upgrade="never")#;options(shiny.fullstacktrace = TRUE)
  # rsconnect::deployApp(appName="tokyo",account="causalmap",forceUpdate = T,lint=F)


getlogs <- function(appName)  {
  library(tidyverse)
  appPath<-file.path(getwd(),"app.R")
  target <- rsconnect:::deploymentTarget(appPath, appName, account="causalmap", NULL, NULL)
  accountDetails <- rsconnect:::accountInfo(target$account)
  client <- rsconnect:::lucidClient(rsconnect:::shinyappsServerInfo()$url, accountDetails)
  application <- rsconnect:::getAppByName(client, accountDetails, target$appName)
  logs <- client$getLogs(application$id, 99999999)
  vlogs <- (logs %>% str_split("\n"))[[1]] %>%
    keep(!str_detect(.,"JAVASCRIPT")) %>%
    keep(.!="")
  vlogs %>% paste0(collapse="\n") %>% str_replace_all("\n\n","\n") %>% writeLines("log.txt" %>% paste0(appName,.))
}
getlogs <- function(appName)  {
  library(tidyverse)
  # appPath<-file.path(getwd(),"app.R")
  # target <- rsconnect:::deploymentTarget(appPath, appName, account="causalmap", NULL, NULL)
  # accountDetails <- rsconnect:::accountInfo(target$account)
  # client <- rsconnect:::lucidClient(rsconnect:::shinyappsServerInfo()$url, accountDetails)
  # application <- rsconnect:::getAppByName(client, accountDetails, target$appName)
  # logs <- client$getLogs(application$id, 99999999)
  # vlogs <- (logs %>% str_split("\n"))[[1]] %>%
  #   keep(!str_detect(.,"JAVASCRIPT")) %>%
  #   keep(.!="")
  # vlogs %>% paste0(collapse="\n") %>% str_replace_all("\n\n","\n") %>% writeLines("log.txt" %>% paste0(appName,.))
showLogs(
  appPath = getwd(),
  appFile = NULL,
  appName = appName,
  account = NULL,
  server = NULL,
  entries = 1000,
  streaming = T
) %>% paste0(collapse="\n") %>% str_replace_all("\n\n","\n") %>% writeLines("log.txt" %>% paste0(appName,.))
}


}
if(F){
  showLogs(appName = "cm3dev")

  getlogs(appName="dummy")
  getlogs(appName="StorySurvey2")
  getlogs(appName="CausalMap2")
  getlogs(appName="CM2test")
  getlogs(appName="tokyo")
  getlogs(appName="cm3dev")

  if(F){
    rsconnect::showLogs( "C:/Users/steve/Dropbox/Projects/StorySurvey2/app.R",appName = "StorySurvey2",account = "causalmap",entries = 999,streaming = T)
    rsconnect::showLogs( "C:/Users/steve/Dropbox/Projects/StorySurvey2/app.R",appName = "StorySurvey2",account = "causalmap",entries = 999,streaming = T)
  }
}
 # rsconnect::deployApp(appName="cm3main",account="causalmap",forceUpdate = T,lint=F)
 rsconnect::deployApp(appName="cm3dev",account="causalmap",forceUpdate = T,lint=F)

