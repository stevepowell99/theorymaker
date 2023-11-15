get_file_access <- function(fil=input$file_select){

  # browser()
  if(is.null(fil))return()
  if(fil=="")return()
  sess$my_files %>% filter(file==fil) %>% pull(access)

}
can_edit <- function(fil=input$file_select){
  if(is.null(fil))return(F)
  if(fil=="")return(T)
  # browser()
  get_file_access(fil) &
    !(as.logical(sess$file$files$locked) %>%
        replace_na(F)) %>%
    replace_zero(F)
}




#
