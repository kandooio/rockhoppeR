getNodes <- function(url, xPathValue) {
  library(rvest)
  library(RCurl)
  x <- read_html(url) %>%
    html_nodes(xpath = xPathValue) %>%
    html_text()
  return(x)
}
