observeEvent(input$interrupt2, {
  browser()

})
output$footer <- renderUI({
  div(

  # browser()
  # choose_sess <- list.files(path="assets/sess",full.names = F) %>% c("",.)
  div(id="notification_area"),
  # div(id="test",class="linkydbl","click me"),

  if(Sys.getenv("USERDOMAIN")==SUPER_ADMIN | Sys.getenv("USERDOMAIN")=="LAPTOP-F57J1GK6")  div(id="foot_div",actionButton("interrupt2","Interrupt"))

  else div(id="foot_div",a("Causal Map | Causal Map Ltd",href="http://causalmap.app"),a(" | Privacy, Terms & Conditions",href="https://causalmap.app/privacy-policy/"))
  )
})
observeEvent(input$save_sess, {
  if(input$save_sess_name==""){
    notify("give a name",5)
    return()
  }

  saveRDS(sess,paste0("assets/sess/",input$save_sess_name))
})

observeEvent(input$intro_cookie_go,{
  intro_cookie()
})
observeEvent(input$intro_explanation_go,{
  intro_explanation()
})
observeEvent(input$intro_survey_go,{
  # intro_survey()
})

# this is so awful because it seems you can't use the fullscreen button inside its own div
# observe({
#   if(!(input$map_box$collapsed %>% replace_null(T)) & input$tabset_right=="explore") show("map_box_fullscreen") else hide("map_box_fullscreen")
# })
observeEvent(input$load_sess, {
  # return()
  # browser()
  if(input$load_sess!=""){
    ss <- readRDS(paste0("assets/sess/",input$load_sess))
    sess$links <- ss$links
    sess$recoded_links <- ss$recoded_links
    sess$text <- ss$text
    sess$graf2<- ss$graf2
    sess$result1 <- ss$result1
    sess$embeddings <- ss$embeddings
    sess$chains <- ss$chains

  }
})


