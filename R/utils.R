#===================================================================
# A bunch of functions that I stole from other people, doing my best to 
# identify source and authors
#
# Author: Catarina Wor 
# Date : October 2018
#===================================================================


#Author: gfplots
read_sql_query <- function(x) {
  #this function reads file containing SQL query

  if (file.exists(system.file("sql", x, package = "era"))) {
    readLines(system.file("sql", x, package = "era"))
  } else {
    stop("The sql file does not exist.")
  }
}


#Author: gfplots
inject_filter <- function(sql_precode, species, sql_code,
                          search_flag = "-- insert species here",
                          conversion_func = common2codes) {
  i <- grep(search_flag, sql_code)
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(conversion_func(species)), ")"
  )
  sql_code
}


test_pkg_load<-function(){
  print(" yay!!!")
}


run_sql <- function(database, query) {
  query <- paste(query, collapse = "\n")
  DBI::dbGetQuery(db_connection(database = database), query)
}

