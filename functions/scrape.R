get_username = function(){
  
  #' Retrieve the user's name
  #' 
  #' Might not work on windows
  #' 
  #' @return str user name
  #' 
  system("whoami", intern = TRUE)
}

scrape_qmd = function(src = "C00_coding.qmd",
                      dst = file.path(get_username(), 
                                      gsub(".qmd", ".R", basename(src), fixed = TRUE))){
  
  #' Scrape the contents of a .qmd to a .R file
  #' 
  #' @param src str the name of the file to scrape
  #' @param dst the destination of the scraped data
  #' @return NULL invisibly
  
  if (!dir.exists(dirname(dst))) path = make_path(dirname(dst))
  
  x = readLines(src)
  index = grep("```{r", x, fixed = TRUE)
  if (length(index) > 0){
    start = seq(from = 1, to = length(index) - 1, by = 2)
    
    ff = file(dst, open = "wt")
    
    for (i in seq_along(start)){
      ix = seq(from = index[start[i]] + 1,
               to = index[start[i]+1] - 1)
      cat(paste("#", substring(x[ix[1] - 1], 4)),sep ="\n", file = ff)
      cat(x[ix], file = ff, sep = "\n")
      cat(file = ff, "\n")
    }
    
    close(ff)
  }
  invisible(NULL)
}
