#' @import gmailr
#' @importFrom httr oauth_app
#' @export
NULL

#' Permite autenticar via pacote 'gmailr' a partir de arquivo 'Renviron'
#'
#' @param oauth_name Padrão 'GOOGLE_OAUTH_NAME'
#' @param oauth_client_id Padrão 'GOOGLE_OAUTH_CLIENT_ID'
#' @param oauth_client_secret_key Padrão 'GOOGLE_OAUTH_CLIENT_SECRET_KEY'
#' @param token Padrão 'GOOGLE_TOKEN'
#' @param email Padrão 'GOOGLE_EMAIL'
#' @param cache Padrão 'GOOGLE_CACHE'
#' @param use_oob Padrão 'FALSE'
#' 
#' @return Autenticação via 'gmailr' à API do 'gmail' a partir de arquivo 'Renviron'
#' 
#' @export
pgm_auth_renv <- function(oauth_name="GOOGLE_OAUTH_NAME", 
                          oauth_client_id="GOOGLE_OAUTH_CLIENT_ID", 
                          oauth_client_secret_key="GOOGLE_OAUTH_CLIENT_SECRET_KEY",
                          token="GOOGLE_TOKEN", 
                          email="GOOGLE_EMAIL", 
                          cache="GOOGLE_CACHE",
                          use_oob = FALSE){
google_app <- oauth_app(
    appname = Sys.getenv(oauth_name),
    key = Sys.getenv(oauth_client_id),
    secret = Sys.getenv(oauth_client_secret_key))
  gm_auth_configure(
    app = google_app, 
    path = Sys.getenv(token))
  gm_auth(
    email = Sys.getenv(email),
    path = Sys.getenv(token),
    cache = Sys.getenv(cache),
    use_oob = use_oob)
  invisible(gm_profile())
}

#' Cria 'data.frame' com a relação de marcadores associados ao e-mail com o respectivo
#' identificador 'id' e tipo 'type'
#'
#' @param label Marcador do gmail para filtro. Se ausente, retorna todos os marcadores
#' @param type  Tipo de marcador para filtro. Se ausente, retorna todos os tipos.
#' Tipos permitidos: 'system' para marcadores de sistema e 'user' para marcadores
#' de usuários
#' @param ... Argumentos a serem repassados à função 'gm_labels' do pacote 'gmailr'
#' 
#' @return 'data.frame' com a relação de marcadores
#' 
#' @export
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

#' Cria 'data.frame' com conversas do gmail (e-mails agrupados por temas) a partir
#' de parâmetros repassados à função 'gm_threads'
#'
#' @param ... Argumentos a serem repassados à função 'gm_threads' do pacote 'gmailr'.
#' Se ausentes, retorna as 100 mais recentes conversas
#' 
#' @return 'data.frame' com a relação de conversas
#' 
#' @export
pgm_threads <- function(...){
  x <- unlist(gm_threads(...))
  x <- data.frame(
    id = x[names(x)=="threads.id"],
    snippet = x[names(x)=="threads.snippet"],
    historyId = x[names(x)=="threads.historyId"],
    stringsAsFactors = FALSE)
  return(x)
}

#' Cria 'data.frame' com e-mails a partir de parâmetros repassados à função 
#' 'gm_messages'
#'
#' @param ... Argumentos a serem repassados à função 'gm_messages' do pacote 
#' 'gmailr'. Se ausente, retorna os 100 mais recentes e-mails, independente da
#' vinculação a determinada conversa
#' 
#' @return 'data.frame' com a relação de e-mails
#' 
#' @export
pgm_messages <- function(...){
  x <- unlist(gm_messages(...))
  x <- data.frame(
     id = x[names(x)=="messages.id"],
     threadId = x[names(x)=="messages.threadId"],
     stringsAsFactors = FALSE)
  return(x)
}

#' Permite a leitura de dados de e-mails a partir de conversas
#' 
#' @param thread_id Identificador de conversa 'threads.id'
#' @param message_id Identificador de e-mail 'messages.threadId' vinculado a
#' determinada conversa
#' 
#' @return 'data.frame' com dados de e-mails vinculados a determinada conversa
#' 
#' @export
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

#' Permite escrever e-mails para envio via API do 'gmail'
#' 
#' @param from Endereço de e-mail do 'gmail' válido. Corresponde ao campo 'De'.
#' @param to Endereço de e-mail do 'gmail' válido. Corresponde ao campo 'Para'.
#' @param subject Assunto do e-mail. Padrão ausente 'NULL'
#' @param body Corpo do e-mail. Padrão ausente 'NULL'
#' @param file Arquivo de anexo. Padrão ausente 'NULL'
#' @param send Se 'TRUE' encaminha o e-mail a partir da função
#' @param thread_id Identificador de conversa a ser vinculado ao e-mail. Padrão
#' ausente 'NULL'
#' 
#' @return 'mime' de e-mail válido para envio ou 'id' de conversa, caso o e-mail 
#' seja enviado a partir da própria função ('send' igual 'TRUE')
#' 
#' @export
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

#' Relaciona e-mails com os respectivos dados a partir de um marcador
#' 
#' @param label Marcador do gmail para filtro. Campo obrigatório.
#' @param num_results Número de resultados de e-mails a partir do filtro 'label'
#' @param num_results_by Número de resultados de e-mails a partir de e-mails
#' e conversas 'all', apenas conversas 'thread' ou apenas e-mails 'message'
#' 
#' @export
pgm_listmessages <- function(label, num_results = 1, num_results_by = "thread"){
  id_label <- pgm_labels(label = label)
  if(nrow(id_label)>1) 
    stop("Enter a unique label")
  if(num_results_by=="all"){
    id_thread <- pgm_threads(label_id = id_label$id, num_results = num_results)
    id_message <- pgm_messages(label_ids = id_label$id, num_results = num_results)
  } else if(num_results_by=="thread"){
    id_thread <- pgm_threads(label_id = id_label$id, num_results = num_results)
    id_message <- pgm_messages(label_ids = id_label$id)
  } else if(num_results_by=="message"){
    id_thread <- pgm_threads(label_id = id_label$id)
    id_message <- pgm_messages(label_ids = id_label$id, num_results = num_results)
  }
  messages <- pgm_read(thread_id = id_thread, message_id = id_message)
  return(messages)
}
