pgm_auth <- function(){
  library(gmailr)
  google_app <- httr::oauth_app(
    appname = Sys.getenv("GOOGLE_OAUTH_NAME"),
    key = Sys.getenv("GOOGLE_OAUTH_CLIENT_ID"),
    secret = Sys.getenv("GOOGLE_OAUTH_CLIENT_SECRET_KEY"))
  gm_auth_configure(
    app = google_app, 
    path = Sys.getenv("GOOGLE_TOKEN"))
  gm_auth(
    email = Sys.getenv("GOOGLE_EMAIL"),
    path = Sys.getenv("GOOGLE_TOKEN"),
    cache = Sys.getenv("GOOGLE_CACHE"),
    use_oob = FALSE)
  return(gm_profile())
}
pgm_labels <- function(label, type, ...){
  labels <- unlist(gm_labels(...))
  x <- data.frame(
    id = labels[names(labels)=="labels.id"],
    name = labels[names(labels)=="labels.name"],
    type = labels[names(labels)=="labels.type"])
  if(!missing(label))
    x <- x[grepl(pattern = label, x = x$name),]
  if(!missing(type))
    x <- x[grepl(pattern = type, x = x$type),]
  return(x)
}
pgm_threads <- function(...){
  x <- unlist(gm_threads(...))
  x <- data.frame(
    id = x[names(x)=="threads.id"],
    snippet = x[names(x)=="threads.snippet"],
    historyId = x[names(x)=="threads.historyId"],
    stringsAsFactors = FALSE)
  return(x)
}
pgm_messages <- function(...){
  x <- unlist(gm_messages(...))
  x <- data.frame(
     id = x[names(x)=="messages.id"],
     threadId = x[names(x)=="messages.threadId"],
     stringsAsFactors = FALSE)
  return(x)
}
pgm_read <- function(thread_id, message_id){
  x <- merge(thread_id, message_id, by.x = "id", by.y = "threadId", suffixes = c("","Message"))
  for(i in 1:nrow(x)){
    x$date[i] <- gm_message(id = x$idMessage[i]) %>% gm_date()
    x$subject[i] <- gm_message(id = x$idMessage[i]) %>% gm_subject()
    att <- gm_message(id = x$idMessage[i]) %>% gm_attachments()
    if(nrow(att)==0){
      x$filename[i] <- as.character("")
      x$type[i] <- as.character("")
      x$size[i] <- as.character("")
      x$date_file[i] <- ""
    } else if (nrow(att)==1){
      x$filename[i] <- as.character(as.factor(att$filename))
      x$type[i] <- as.character(as.factor(att$type))
      x$size[i] <- as.character(as.factor(att$size))
      if(grepl(pattern = "\\d{4}\\-\\d{2}\\-\\d{2}", x = x$filename[i])){
        dt <- regexpr("\\d{4}\\-\\d{2}\\-\\d{2}", x$filename[i])
        x$date_file[i] <- regmatches(x$filename[i], dt)
      } else {
        x$date_file[i] <- ""
      }
    } else {
      x$filename[i] <- paste(as.character(as.factor(att$filename)), collapse = ", ")
      x$type[i] <- paste(as.character(as.factor(att$type)), collapse = ", ")
      x$size[i] <- paste(as.character(as.factor(att$size)), collapse = ", ")
      x$date_file[i] <- ""
    }
    }
  x$date_file <- as.Date(x$date_file)
  x <- x[,!colnames(x) %in% c("snippet", "threadId")]
  x <- x[order(x$date_file),]
  return(x)
}
pgm_write <- function(from, to, subject = NULL, body = NULL, file = NULL, send = FALSE, thread_id = NULL){
  x <- gm_mime() %>%
    gm_from(from) %>%
    gm_to(to) %>%
    gm_subject(subject) %>%
    gm_html_body(body)
  if(!is.null(file)){
    for(i in 1:length(file)){
      x <- x %>% 
        gm_attach_file(filename = file[i])
    }
  }
  if(send){
    if(is.null(thread_id)){
      x <- gm_send_message(mail = x)
    } else {
      x <- gm_send_message(mail = x, thread_id = thread_id)
    }
  }
  return(x)
}
