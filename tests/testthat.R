library(testthat)
library(gmailr)
library(ppgmailr)

context("")
test_that("Autenticação", pgm_auth())
test_that("Extrair código marcador", pgm_labels(label = "INBOX"))
test_that("Extrair tópicos", pgm_threads(label_id = pgm_labels(label = "INBOX")$id, num_results = 1))
test_that("Extrair mensagens", pgm_messages(label_ids = pgm_labels(label = "INBOX")$id$id, num_results = 1))
test_that("Extrair dados das mensagens", 
          pgm_read(thread_id = pgm_threads(label_id = pgm_labels(label = "INBOX")$id, num_results = 1), 
                   message_id = pgm_messages(label_ids = pgm_labels(label = "INBOX")$id$id, num_results = 1)))
test_that("Escrever e enviar mensagens", 
          pgm_write(from = gm_profile()$email, to = gm_profile()$email, subject = "ppgmailr test", send = TRUE))
test_that("Listar mensagens",
          pgm_listmessages(label = "INBOX"))
test_that("Criar tabela html para o email",
          pgm_tablehtml(x = data.frame("id"=1, "name"="tablehtml"), width = c(200,300)))