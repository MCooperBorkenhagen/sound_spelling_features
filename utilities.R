distance_matrix_to_long = function(m, words = NA){
  
  #' Turns a distance matrix into a long data format
  #' @param m A distance matrix of class "dist"
  #' @param words The words to be used as labels for the margins of m
  #' @returns A tibble containing a row for each unique combination of 
  #' marginal elements of m. Columns starting with word_ identify the
  #' pairs and the distance provided is that which is supplied in 
  #' observations contained in m.
  
  upper_triangle <- as.matrix(m)[upper.tri(m, diag = TRUE)]
  
  # Get the row and column indices for the upper triangle
  indices <- which(upper.tri(m, diag = TRUE), arr.ind = TRUE)
  
  # Create a data frame with the indices and distances
  long_format <- tibble(row_index = indices[, "row"],
                        column_index = indices[, "col"],
                        distance = upper_triangle)
  
  if (!any(is.na(words))){
    
    indices = tibble(word = words, index = 1:length(words))
    
    long_format = long_format %>% 
      left_join(indices, by = c("row_index" = "index")) %>% 
      left_join(indices, by = c("column_index" = "index")) %>% 
      select(word_1 = word.x, word_2 = word.y, distance)
    
    return(long_format)}}
