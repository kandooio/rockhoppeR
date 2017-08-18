# getNodes uses the rvest and RCurl libraries to extract xPath Values from a given URL
# Useful for extracting HTML content from websites.

getNodes <- function(url, xPathValue) {
  library(rvest)
  library(RCurl)
  x <- read_html(url) %>%
    html_nodes(xpath = xPathValue) %>%
    html_text()
  return(x)
}
