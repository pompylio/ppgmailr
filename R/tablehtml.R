pgm_tablehtml <- function(obj, width = NULL, bg_title='#374049', color_text="white", bg_column='#e2e6e9', ...){
  x <- tableHTML::tableHTML(obj = obj, rownames = FALSE, widths = width, ...)
  x <- tableHTML::add_css_column(tableHTML = x, css = list('text-align', 'center'), columns = 1)
  x <- tableHTML::add_css_column(tableHTML = x, css = list('background-color', bg_column), columns = 1)
  x <- tableHTML::add_css_row(tableHTML = x, css = list(c('background-color','color'), c(bg_title,color_text)), rows = 1)
  x <- tableHTML::add_css_row(tableHTML = x, css = list('background-color', 'white'), rows = tableHTML::odd(1:nrow(obj)+1))
  x <- tableHTML::add_css_row(tableHTML = x, css = list('background-color', 'white'), rows = tableHTML::even(1:nrow(obj)+1))
  return(x)
}