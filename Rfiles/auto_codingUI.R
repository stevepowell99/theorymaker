# UI ----------------------------------------------------------------------
output$auto_codingUI <- renderUI({


  shinyalert(
    title = "theorymaker3",
    text = div(
      p("This is a very experimental service provided by ",a(href="https://causalmap.app"," Causal Map Ltd.")),
      p("At the moment you can not save your work, so be sure to take screenshots of your diagrams and/or download them using the `Save SVG` button."),
      p("If you want to recreate the same diagrams at a later time, you will have to copy and paste the prompts you used and keep them somewhere safe."),
      hr(),
      p("The app contains a few example prompts to get started, or you can start with your own."),
      p("You may find it better to first create just one part of your diagram and then add more parts in later steps."),
      hr(),
      p("Do NOT use theorymaker for any information which is sensitive or which identifies individuals.")
    )%>% as.character,
    size = "l",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    html = T,
    # type = "success",
    showConfirmButton = T,
    showCancelButton = FALSE,
    confirmButtonText = "Click to agree",
    # confirmButtonCol = "#AEDEF4",
    timer = 30000,
    imageUrl = "https://live.staticflickr.com/65535/53302700033_43c775b585_o.png",
    imageWidth = 700,
    imageHeight = 300,
    animation = F,
    inputId = "thinking"
  )


  # if(!sess$is_admin)return(div(
  #   h3("You do not have permission."),
  #   p("Contact Causal Map to join the waiting list for the private beta of Causal Map AI.")
  # ))
#  if(is.null(current_statements()))return()
if(F){

  previous_prompts_df <-
    tbl(conn,"cm3usage") %>%
    filter(user==local(sess$user) |user=="testing" | local(sess$is_admin)) %>%
    filter(file==local(input$file_select)) %>%
    select(-statements) %>% #FIXME seems to make it last forever
    collect %>%
    arrange(desc(time_stamp)) %>%
    select(prompt,time_stamp) %>%
    distinct(prompt,.keep_all = T)

  previous_prompts <- previous_prompts_df$prompt
  names(previous_prompts) <- previous_prompts_df$time_stamp

}


  div(id = "Autocode",

fluidRow(
    column(12,
           box(solidHeader=T,status="info",width=12,title="Prompt",collapsible = T,

           # selectInput(
           #   "select_auto_coding_prompt_previous",
           #   "Previous prompts",
           #   choices = previous_prompts
           #   # ,
           #   # selected = prompts[1]
           # ) %>% div(class = "inline"),
           selectInput(
             "select_auto_coding_prompt_1",
             "Pick an example or type your own",
             choices = prompts
             ,
             selected = prompts[sample(seq_along(prompts),1)]
           ) %>% div(class = "inline"),
           textAreaInput(
             "auto_coding_prompt_1",
             NULL,
             width = "100%",
             height = "300px"

           )
           )
           ,
           div(id = "main",
               div(
                 actionButton("autocoding_go", "Go",class="big green")
               ))
    ),
    box(solidHeader=T,status="info",width=12,title="Result",collapsible = T,collapsed = T,
        p("If you know what you are doing, you can edit this and the map will change. But the AI does not know you have made the change."),
        textAreaInput(inputId = "result_p_1","Result",value = map1,
                         width = "100%"
           )
    )           ,
    box(solidHeader=T,status="info",width=12,title="Settings",collapsible = T

    )

)
  )
})


observe({
  if(is.null(sess$file)) return()
  if(!(flags$init_statements)) return()
  if(!use_sql) return()
  sess$file$links
  flags$init_statements
  # browser()
  previous_prompts_df <-
    tbl(conn,"cm3usage") %>%
    filter(user==local(sess$user) |user=="testing" | local(sess$is_admin)) %>%
    filter(file==local(input$file_select)) %>%
    select(-statements) %>% #FIXME seems to make it last forever
    collect %>%
    arrange(desc(time_stamp)) %>%
    select(prompt,time_stamp) %>%
    distinct(prompt,.keep_all = T)

  previous_prompts <- previous_prompts_df$prompt
  names(previous_prompts) <- previous_prompts_df$time_stamp

  updateSelectInput(session,"select_auto_coding_prompt_previous",choices=previous_prompts) %>% delay(5000,.)
})
observeEvent(ignoreNULL = T,ignoreInit = T,input$select_auto_coding_prompt_1,{

  # browser()
  #sess$messages <- NULL
  updateTextAreaInput(session=session,"auto_coding_prompt_1",
                      value=readLines(paste0("assets/prompts/",input$select_auto_coding_prompt_1)) %>% suppressWarnings() %>% collap("\n")
  )
})
observeEvent(ignoreNULL = T,input$select_auto_coding_prompt_previous,{

  updateTextAreaInput(session=session,"auto_coding_prompt_1",
                      value=input$select_auto_coding_prompt_previous
  )
})

observeEvent(input$auto_coding_prompt_1, {
  #  shinyjs::html(paste0("result_p_1"), "")
})
#

observeEvent(input$text_1, {
  shinyjs::html(paste0("result_p_1"), "")
})

# observeEvent(can_edit(), {
#   if(!can_edit()) shinyjs::disable("result_go") else shinyjs::enable("result_go")
# })
#

gifs <- list(
  "https://media.tenor.com/mL_vvv35zRwAAAAC/pattern-abstract.gif",
  "https://media.tenor.com/xGFOS0iEhPkAAAAd/galaxy-hypnotic.gif",
  "https://media.tenor.com/62yG4csYM2QAAAAC/fractal.gif",
  "https://media.tenor.com/BXhyJTPVm2oAAAAd/abstract.gif",
  "https://media.tenor.com/mm4JhI3kgdEAAAAd/fractal.gif",
  "https://upload.wikimedia.org/wikipedia/commons/a/a4/Mandelbrot_sequence_new.gif",
  "https://i.gifer.com/9T2X.gif",
  "https://media1.giphy.com/media/qoJ9sZu2Xui9a/giphy.gif?cid=ecf05e47oanhquyhopodpqnztq4as6ux7wvr6yrn5hzbz7m3&ep=v1_gifs_search&rid=giphy.gif&ct=g",
  "https://media.tenor.com/T_abp3SBPd0AAAAd/fractal-julia-set.gif"
)



observeEvent(input$autocoding_go,ignoreNULL = T,ignoreInit = T, {

  prompt <- input$auto_coding_prompt_1 %>% remove_lines_starting_with

  if(is.null(sess$messages)){
    prompt <-
      prompt %>%
    paste0("\nGive the Graphviz DOT syntax for your diagram. Unless I just told you otherwise, your diagram should use Arial font throughout. I will refer to the nodes as 'boxes' or 'factors' and the edges as 'links' or 'arrows'.
           The boxes must have rounded edges, with a very light blue background, and the arrows should be light gray. If the labels are more than a few words, wrap them using line breaks. If you use link labels, use Arial font and wrap using line breaks.
           You may use emojis to highlight boxes or links which have things in common, e.g. the same stakeholder or the same kind of activity.
           Also provide a short narrative description of the diagram." )
  }

  sess$messages <-
    sess$messages %>%
    append(list(list(role="user",content=prompt))) %>%
    compact
  notify("Having a think, please be patient")
  think_id <- shinyalert(
    title = "Thinking, designing, drawing...",
    text = "This usually takes about 30 seconds. Why don't you have a stretch or look out of the window for a minute?",
    size = "l",
    closeOnEsc = T,
    closeOnClickOutside = T,
    html = FALSE,
    # type = "success",
    showConfirmButton = F,
    showCancelButton = FALSE,
    # confirmButtonText = "OK",
    # confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = gifs[sample(seq_along(gifs),1)],
    imageWidth = 600,
    imageHeight = 600,
    animation = TRUE,inputId = "thinking"
  )
  response <- ask_chatgpt(sess$messages,api_key = api_key,model = "gpt-4-1106-preview")
  # browser()

  updateTextAreaInput(session=session,"auto_coding_prompt_1",
                      value="Now continue your conversation to improve the diagram. Replace this text with your additional tweaks then press Go again. For example, ask to change the format or add more detail or remove specific factors or links.") %>% delay(5000,.)

  res <- response$content %>% str_split("\n") %>% pluck(1)

  if(any(str_detect(res,"```"))){
    boundaries <- which(str_detect(res,"```"))
    if(length(boundaries)!=2) {notify("boundaries error",4);return()}
    legend1 <- res[1:(boundaries[1]-1)]
    legend2 <- res[(boundaries[2]:length(res))]
    legend <- c(legend1,legend2) %>% na.omit %>% collap
    res <- res[(boundaries[1]+1):(boundaries[2]-1)] %>% collap
    # browser()
    control$legend <- legend
  } else res <- res %>% collap %>% str_replace_all("'","")


  sess$messages <-
    sess$messages %>%
    append(list(list(role="assistant",content=res)))

  closeAlert(id=think_id)


  if(T){

  usage <- list()

  usage$time_stamp <- time_stamp()
  usage$tokens <- sess$messages %>% unlist %>% nchar %>% sum %>% `/`(7) %>% round(0)
  usage$file="theorymaker"#input$file_select
  usage$prompt=input$select_auto_coding_prompt_1
  if(exists("sess"))usage$user=sess$user else usage$user=("testing")
# browser()
  if(use_sql){
  dbWriteTable(conn=conn,name="cm3usage",local(as_tibble(usage)),append=T)
  control$my_usage <- tbl(conn,"cm3usage") %>% filter(user==local(sess$user)) %>% pull(tokens) %>% sum(na.rm=T)
  #response <- attr(links_new,"response")
  }
  }
  updateTextAreaInput(session = session,"result_p_1",value=res %>% collap)







})
