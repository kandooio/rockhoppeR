# capCollapse Function looks for consecutive words beginning with capital letters and collapses them into a single word.

capCollapse <- function(string){
  temp <- gsub('([A-Z]\\w*)', '\\1\\$MARK\\$', string)
  output <- gsub('(?<=\\$MARK\\$)\\s+(?=[A-Z])', '', temp, perl=TRUE)
  output <- gsub('\\$MARK\\$', '', output)
  return(output)
}

capCollapse("The current president of the United States Donald Trump")
