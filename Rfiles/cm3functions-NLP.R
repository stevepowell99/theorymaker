ask_chatgpt <- function(messages,api_key,model="gpt-4") {
  # browser()
  response <- POST(
    url =  "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = messages,
      # stream =T,

      temperature=0,
      presence_penalty=0,frequency_penalty=0
    )
  )

  # browser()
  notify("Received response",1)
  (content(response)$choices[[1]]$message)
}
