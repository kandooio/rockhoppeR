# getSQL
# Custom function to run SQL queries via a JDBC connection.

getSQL <- function(sql){
  library(RJDBC)
  vDriver <- JDBC(driverClass="[INSERT_DRIVER_CLASSS]", classPath="[PATH_TO_JAR_FILE]")
  vertica <- dbConnect(vDriver, "[INSERT_HOST]", "[INSERT_USER]", "[INSERT_PASSWORD]") 
  df <- dbGetQuery(vertica, sql)
  return(df)
}
