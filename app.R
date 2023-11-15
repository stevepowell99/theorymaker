is_laptop <- (Sys.getenv("USERDOMAIN") == "STEVE-P")

options(shiny.autoreload = F)
options(shiny.maxRequestSize=50*1024^2)

library(aws.s3)
library(bslib)
library(bsicons)
library(configr)
library(DT)
library(dbplyr)
library(dplyr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(DBI)
library(httr) # just for POST
library(httr2)
library(igraph)
library(jsonlite)
library(shinyjqui)
library(shinyalert)
library(polished)
library(RMySQL)
library(RMariaDB)
library(readxl)
library(shiny)
library(stringr)
library(stringdist)#for afind
library(stringi)
library(shinyjs)
library(snapper)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinybusy)
library(tidyverse)
library(tidyjson)
library(writexl)
library(xml2)
SUPER_ADMIN <- "STEVE-P"
SUPER_USER <- "steve@pogol.net"
if(Sys.getenv("USERDOMAIN")==SUPER_ADMIN)options(shiny.port = 3344) else options(shiny.port = 3839)


# sourcing ----------------------------------------------------------------

source("Rfiles/cm3functions.R")
source("constants.r")




ui = uiOutput("whole_ui")

server <- function(input, output, session) {
  # sourcing ----------------------------------------------------------------

  message("sourcing server files ....")
  source("Rfiles/cm3functions-NLP.R")
  message("sourcing....")
  source("Rfiles/whole_UI.R", local = T)
  source("Rfiles/auto_codingUI.R", local = T)
  source("Rfiles/footer.R", local = T)
  source("Rfiles/map.R", local = T)
  message("sourced....")

  config = configr::read.config("config.yml")$default

  api_key <- config$api_key


  conn <- DBI::dbConnect(
    #RMySQL::MySQL(),
    drv = RMariaDB::MariaDB(),
    load_data_local_infile = T,
    timeout=6000,
    reconnect=T,#FIXME could be dangerous
    encoding="utf8mb4",

    # drv = RMySQL::MySQL(max.con=1000, fetch.default.rec=1000),
    idleTimeout=900000,#15 minutes
    interactive_timeout=900000,#15 minutes
    wait_timeout=900000,#15 minutes
    dbname = config$sql_cm$dbname,
    host = config$sql_cm$host,
    port = config$sql_cm$port,
    username = config$sql_cm$username,
    password = config$sql_cm$password,
    mysql=F
  )



  session$onSessionEnded(function() {
    sess <- NULL
  })

  timer2 <- reactiveTimer(1000 * 2) # time unit in milliseconds
  timer60 <- reactiveTimer(1000 * 60) # time unit in milliseconds
  timer12 <- reactiveTimer(1000 * 12) # time unit in milliseconds



  sess <- reactiveValues()
  sess$messages <- NULL

  control <- reactiveValues()
  flags <- reactiveValues() # just for flags whether things have been done or not

  flags$init_statements <- F
  flags$init_tables <- F

  message("initialised....")

  observe({
    sess$user_all <-
      session$userData$user() %>% replace_null(list(email = "no_user", is_admin =F))
    sess$user <- sess$user_all$email
#    sess$user <- "steve@pogol.net" ###fixme
    control$my_usage <- tbl(conn,"cm3usage") %>% filter(user==local(sess$user)) %>% pull(tokens) %>% sum(na.rm=T)


    sess$is_admin <- sess$user_all$is_admin

    sess$q <- getQueryString()
    sess$s <- sess$q[["s"]]


  })


# UI ----------------------------------------------------------------------



# not used at moment

  observeEvent(input$file_select, {
    message("observing file select ....")

    fil <-
      input$file_select

    if(fil=="")updateSelectInput(session,"file_select",selected = "example-file")
    sess$file <- get_file(fil,conn)

    sess$file_old <- sess$file

    control$link_choices <- isolate(sess$file$links) %>% add_link_counts_simple() %>% colnames %>% c("none",.)


    message("observed file select ....")



  })






  observeEvent(input$sign_out, {
    # browser()
    sign_out_from_shiny(session)
    session$reload()

  })

}

app_config <- config::get()

polished::polished_config(
  app_name = "StorySurvey",
  api_key = app_config$polished_api_key,
  is_invite_required = FALSE,
  is_auth_required = F,
  is_two_fa_required = F,
  firebase_config = list(
    apiKey = app_config$firebase_apiKey,
    authDomain = app_config$firebase_authDomain,
    projectId = app_config$firebase_projectId
  ),
  sign_in_providers = c("google", "email")
)

ui_secure <- secure_ui(
  ui,
  sign_in_page_ui = polished::sign_in_ui_default(
    company_name = "Causal Map",
    logo_top = tags$div(
      style = "width: 300px; max-width: 100%; color: #FFF;",
      class = "text-center",
      h1("theorymaker3", style = "margin-bottom: 0; margin-top: 30px;"),
      p(
        "Sign in with a google account or an email address."
      )
    ),
    color = "#344"
    ,
    terms_and_privacy_footer = a("Terms and conditions at https://causalmap.app", href =
                                   "https://www.causalmap.app/terms-and-conditions-for-the-causal-map-app")
  )
)
server_secure <- secure_server(server)
shinyApp(ui_secure,server_secure, enableBookmarking = "url")


