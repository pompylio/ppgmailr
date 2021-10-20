#' Tabela padrão para envio no corpo do e-mail a ser criado a partir do pacote
#' 'ppgmailr'
#' 
#' @param obj Objeto a ser convertido em tabela 'html'
#' @param width Vetor com o tamanho de cada coluna da tabela em 'pixels'. Valor
#' ausente 'NULL' por padrão
#' @param bg_title Cor de fundo do título. Valor padrão '#374049'
#' @param color_title Cor do texto. Valor padrão 'white' ou '#ffffff'
#' @param bg_column Cor de fundo da primeira coluna da tabela. Valor padrão #e2e6e9'
#' @param ... Outros parâmetros para a função 'tableHTML'
#' 
#' @return Tabela em html a ser usada no corpo do e-mail
#' 
#' @importFrom tableHTML tableHTML add_css_column add_css_row odd even
#' 
#' @export
pgm_tablehtml <- function(obj, width = NULL, bg_title='#374049', color_title="white", bg_column='#e2e6e9', ...){
  x <- tableHTML::tableHTML(obj = obj, rownames = FALSE, widths = width, ...)
  x <- tableHTML::add_css_column(tableHTML = x, css = list('text-align', 'center'), columns = 1)
  x <- tableHTML::add_css_column(tableHTML = x, css = list('background-color', bg_column), columns = 1)
  x <- tableHTML::add_css_row(tableHTML = x, css = list(c('background-color','color'), c(bg_title, color_title)), rows = 1)
  x <- tableHTML::add_css_row(tableHTML = x, css = list('background-color', 'white'), rows = tableHTML::odd(1:nrow(obj)+1))
  x <- tableHTML::add_css_row(tableHTML = x, css = list('background-color', 'white'), rows = tableHTML::even(1:nrow(obj)+1))
  return(x)
}