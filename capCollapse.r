# capCollapse Function looks for consecutive words beginning with capital letters and collapses them into a single word. Useful for grouping terms together for use in text mining.

# Example
# capCollapse("The current president of the United States is Donald Trump")
# should return
# [1] The current president of the UnitedStates is DonaldTrump

capCollapse <- function(string){
  temp <- gsub('([A-Z]\\w*)', '\\1\\$MARK\\$', string)
  output <- gsub('(?<=\\$MARK\\$)\\s+(?=[A-Z])', '', temp, perl=TRUE)
  output <- gsub('\\$MARK\\$', '', output)
  return(output)
}

