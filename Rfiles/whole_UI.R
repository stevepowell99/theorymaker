output$whole_ui <- renderUI({
  if(use_authentication){
  if(is.null((sess$q)))return()
  if(is.null((sess$user)))return("Sign in")
}
  message("rendering wholeUI....")

  page_fluid(
    title = ("theorymaker"),

    theme = bs_theme(preset = "lumen"),

    tags$head(
includeScript("www/panzoom.min.js"),
tags$link(
  rel = "stylesheet",
  type = "text/css",
  href = "styles/app-styles.css"
),
tags$link(
  rel = "stylesheet",
  type = "text/css",
  href = "styles/fades.css"
),
shinycookie::initShinyCookie("my_api_key_cookie"),#  # Initialize shinycookie

useShinyjs(),
useShinydashboardPlus()
    )
,


add_busy_bar(color = "#8fb8ff")

,
if("no_user"==((sess$user)))return(
  div(style="text-align: center;
    margin-top: 30vh;
    font-family: sans-serif;",
    h1("Welcome to theorymaker"),

    a("Sign in here",href=".?page=sign_in")
  )
) else {
  fluidRow(


    column(4
         ,
         div(id="logo",href="https://causalmap.app",img(src="img/strapline.png",height="90px"))
         ,
         uiOutput("auto_codingUI")

  ),
  column(8,

         div(id = "right_panel",
             tabsetPanel(id = "tabset_right",
                         tabPanel(title="Map",
                                  icon=icon("location-arrow"),
                                  uiOutput("map_formats"),
                                  uiOutput("map_box"),
                                  div(id="legend",control$legend %>% replace_null(""))

                         )
                         ,
                         if(use_sql | use_authentication)tabPanel(title="Account",value="Account",icon=icon("user"),


                                  div(
                                    id = "sign_in_out",
                                    class = "inline",
                                    div(if (session$userData$user()$email_verified %>% replace_null(F))
                                      actionButton(
                                        "sign_out",
                                        paste0("Sign out: ",sess$user),
                                        icon = icon("sign-out-alt"),
                                        class = "pull-right"
                                      )
                                      else
                                        a(href = ".?page=sign_in", span(icon("sign-in-alt"), "Sign in"), class =
                                            "btn btn-default pull-right")
                                    ),
                                    div(
                                      h3("OpenAI API responses and usage"),
                                      p("How much fuel do you have left?"),
                                      p("When you run out of fuel you won't be able to use theorymaker. Sorry. Please contact hello@causalmap.app."),
                                      shinyWidgets::progressBar(
                                        id="fuel",
                                        max(0,100-(control$my_usage/100)),
                                        total = NULL,
                                        display_pct = FALSE,
                                        size = NULL,
                                        status = NULL,
                                        striped = FALSE,
                                        title = NULL,
                                        range_value = NULL,
                                        commas = TRUE,
                                        unit_mark = "%"
                                      ),
                                      input_switch("usage_toggle","Show prompt info"),
                                      p("Note this assumes all tokens are output tokens, real dollar cost will be a bit less."),
                                      uiOutput("usage_table")
                                    )
                                  )
                         )

             ))
  )

)}
,
  uiOutput("footer")
  )
})

output$usage_table <- renderUI({

  input$autocoding_go

  # just providing very rough cost calculation


  tbl(conn,"cm3usage") %>%
    filter(user==local(sess$user) |user=="testing" | local(sess$is_admin)) %>%
    select(-statements) %>% #FIXME seems to make it last forever
    select(-event_type) %>% #FIXME seems to make it last forever
    collect %>%
    arrange((time_stamp)) %>%
    mutate(dollars=ifelse(model=="3",tokens*0.000002,tokens*0.00006)%>% signif(4)) %>%
    mutate(cumulative_dollars=cumsum(dollars %>% replace_na(0)) %>% signif(4)) %>%
    {if(!input$usage_toggle)select(.,-prompt,-response) else select(.,-dollars,-cumulative_dollars,-tokens)} %>%
    arrange(desc(time_stamp)) %>%
    select(-row_names) %>%
    datatable(
    )

})
