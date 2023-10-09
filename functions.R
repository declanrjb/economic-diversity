retrieve_freshman <- function(college_name,admission_reference) {
  temp_enrollment <- admission_reference %>% filter(College == college_name)
  
  if (dim(temp_enrollment)[1] == 0) {
    return(NA)
  }
  
  if ("ENRLFT" %in% colnames(temp_enrollment)) {
    grand_total <- soft_parse_number(temp_enrollment[['ENRLFT']])
  } else {
    grand_total <- soft_parse_number(temp_enrollment[['ENRLFTM']]) + soft_parse_number(temp_enrollment[['ENRLFTW']])
  }
  
  if (length(grand_total) > 0) {
    grand_total <- grand_total[1] 
  }
  return(grand_total)
}

make_freshman_retriever <- function(year) {
  if (year == 2020) {
    freshman_retriever <- function(college_name) {
      result <- retrieve_freshman(college_name,admission_2020)
      return(result)
    }
  } else if (year == 2010) {
    freshman_retriever <- function(college_name) {
      result <- retrieve_freshman(college_name,admission_2010)
      return(result)
    }
  }
  return(freshman_retriever)
}

soft_parse_number <- function(x) {
  if (is.character(x)) {
    suppressWarnings({
      return(parse_number(x))
    })
  } else if (is.numeric(x)) {
    return(x)
  } else {
    return(NA)
  }
}

calculate_simpsons <- function(pell_students,all_students) {
  non_pell_students <- all_students - pell_students
  modified_pell <- pell_students * (pell_students - 1)
  modified_non_pell <- non_pell_students * (non_pell_students - 1)
  simpsons_sum <- modified_pell + modified_non_pell
  divisor <- all_students * (all_students - 1)
  simpsons_outcome <- simpsons_sum / divisor
  simpsons_outcome <- 1 - simpsons_outcome
  return(simpsons_outcome)
}

soft_match <- function(original_vector,input,output) {
  result <- output[match(original_vector,input)]
  na_indices <- which(is.na(result))
  result[na_indices] <- original_vector[na_indices]
  return(result)
}

tidy_up_string_vector <- function(string_vector) {
  string_vector <- gsub("[[:punct:]]","",string_vector)
  string_vector <- str_squish(string_vector)
  string_vector <- str_to_lower(string_vector)
  return(string_vector)
}