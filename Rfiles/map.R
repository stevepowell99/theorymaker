# maps --------------------------------------------------------------------

observe({
  if(is.null(input$file_select))return()
})

output$map_formats <- renderUI({
  dropdown(icon=span("Formatting"),actionButton("map_button",label=NULL),
                     # placement="left",
                     div(
                       h3("Label wrapping")
                       ,
                       sliderTextInput("map_wrap_factor_labels","Factors",choices=c(3:100,"Off"),selected="22",grid=F,width="220") %>% div(class="inline")
                       ,
                       sliderTextInput("map_wrap_link_labels","Links",choices=c(3:100,"Off"),selected="22",grid=F,width="220") %>% div(class="inline")
                       ,
                       h3("Factor spacing")
                       ,
                       sliderTextInput("map_ranksep","Horizontal",choices=c((1:30)/2),selected="3",grid=F,width="220") %>% div(class="inline")
                       ,
                       sliderTextInput("map_nodesep","Vertical",choices=c((1:15)/2),selected="2",grid=F,width="220") %>% div(class="inline")


                     )
  )
})
output$map_box <- renderUI({
      card(
        full_screen = TRUE,
        card_header(
        ),


        # div("here is the map")
        uiOutput("ss_map_holder")# %>% div(class="inline",style="width:100%")

      )
  # )
})

output$ss_map_holder <- renderUI({
  div(  load_snapper(),## SEEMS TO BE NECESSARY TO PUT THIS NEAR THE BUTTON
        actionGroupButtons(
          inputIds = c("zoomout", "zoomin", "reset"),
          labels = list(icon("minus"), icon("plus"), "Reset"),
          status = "primary"
        ) %>% div(class="inline"),
        input_switch("map_height_toggle","Fit height?",value = F) %>% div(class="inline",id="map_height_toggle_holder"),
        downloadButton("save_grv","Save svg") %>% div(class="inline",title="Save a very high-quality vector image to your desktop which you can include in a report\n(Tip: Click 'Insert / Picture' in Microsoft Word)"),

        grVizOutput("ss_map",height = "100%",width="100%")
        ,
        tags$script(HTML(panzoomjs)
        )

  )
})



observeEvent(input$map_height_toggle, {
  # browser()
  if(input$map_height_toggle)shinyjs::addCssClass("ss_map","shrunk_map")else shinyjs::removeCssClass("ss_map","shrunk_map")
})


# map ---------------------------------------------------------------------






output$ss_map <- renderGrViz({

  sess$messages
  input$result_p_1


  control$map <- make_print_map3(
  )
  control$map

})

make_print_map3 <- function(){
  # browser()
  if(is.null(sess$messages))return()
  grViz(input$result_p_1 %>% str_replace_all("'",""))
  # grViz(sess$messages[[length(sess$messages)]]$content)
}

output$save_grv <- downloadHandler(


  filename = function() {
    paste("map", "-",Sys.Date(), ".svg", sep="")
    # paste(input$file_select, "-",Sys.Date(), ".svg", sep="")
  },
  content = function(file) {
    if(F){

      html("temp_clipboard",a(href=make_URL_from_shortlink(link),link) %>% as.character)%>% delay(1500,.)
      js$copyRichText(paste0("temp_clipboard")) %>% delay(2000,.)
      notify("Copied to clipboard",3) %>% delay(2500,.)
      link <- add_shortlink(notify_clipboard=F)
    }


    control$map     %>%
      export_svg() %>%
      read_xml() %>%
      write_xml(file)
    message("made svg")
    # browser()
    if(F){

      tmp <- readLines(file)
      nlines <- length(tmp)
      new2 <- paste0('<text text-anchor="left" x="6" y="-6" font-family="Helvetica,sans-Serif" font-size="12.00" fill="#ababab">',link,'</text>')
      message("exporting svg")
      notify("Exported svg. Now you can paste the file into a word processing document.
           A shortlink has been created and the link number is printed at the bottom left of the image file.",3)
      writeLines(c(tmp[1:(nlines-4)],new2,tmp[(nlines-3):nlines]),file)
    }
  }
)

observeEvent(flags$init_map %>% c(sess$bookmark),{
  if(is.null(sess$bookmark))return()
  if(is.null(flags$init_map))return()
  # browser()
  sess$bookmark %>% keep_at(~str_detect(.,"^map")) %>%
    imap(~{
      message(paste0("Updating ",.y," to ",.x))
      if(is_logical(.x)){
        update_switch(session=session,.y,value = .x) %>% delay(2000,.)

      } else if(str_detect(.y,"_type")){
        updateRadioGroupButtons(session,.y,selected = .x) %>% delay(2000,.)

      } else if(str_detect(.y,"_wrap")){
        # browser()
        if(.x!="Off").x <- .x %>% as.numeric
        updateSliderInput(session,.y,value = .x) %>% delay(2000,.)
      } else if(str_detect(.y,"_ranksep|_nodesep")){
        # browser()
        if(.x!="Off").x <- .x %>% as.numeric
        updateSliderTextInput(session,.y,selected  = .x) %>% delay(2000,.)

         } else if(str_detect(.y,"hashtag|tracing")){
          updateSelectInput(session,.y,selected = .x) %>% delay(2000,.)
        } else {
          # browser()
          updateSelectInput(session,.y,selected = .x) %>% delay(2000,.)
          # updateSliderTextInput(session,inputId= .y,selected = .x) %>% delay(2000,.)

        }

    })

  #updateSliderInput(session,names(widget),value = widget)



})
