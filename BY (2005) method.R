#### Code to implement BY (2005)
# The code will return the BH-corrected Z-score, which can be used to create confidence
# intervals adjusted for multiple testing 

# To compute the adj CI, use the formula: point est +/- [SE * Z-score]
# User can indicate whether the multiple tests are independent or dependent 


# Load packages
library(tidyverse)



by_2005_ci <- function(p_vals_vec, q_val, indep_tests = TRUE){
  
  # Part 1: Set up tbl w/ p-vals in ascending order ---------------------------------
  
  num_tests <- length(p_vals_vec)
  
  # Set up the original tbl of p-vals (in the original order -- as inputted by user)
  original_tbl <- tibble(p_val = p_vals_vec) %>%
    dplyr::mutate(original_order = dplyr::row_number())
  
  # Set up the tbl w/ p-vals in ascending order (smallest at top)
  sorted_tbl <- original_tbl %>%
    # Sort in ascending order 
    dplyr::arrange(p_val) %>%
    # Create a new var denoting rank (ascending: rank 1 = smallest p-val)
    dplyr::mutate(rank_ascending = dplyr::row_number())
  
  
  # Part 2: compute the BH crit vals: case 1 (indep tests); case 2 (dep tests) ---------- 
  
  if(indep_tests == TRUE){
    
    sorted_tbl <- sorted_tbl %>%
      dplyr::mutate(
        bh_critical_val = (rank_ascending/num_tests)*q_val,
        # indicator var: p-val is less than or equal to critical val
        p_val_smaller = ifelse(p_val <= bh_critical_val, "True", "False")
      )
    
  } else { # i.e., if tests are dependent 
    
    # Compute c_m_val
    c_m_vec <- vector(mode = "double", length = num_tests)
    
    for (i in 1:num_tests) {
      
      c_m_vec[i] <- (1/i)}
    
    c_m_val <- sum(c_m_vec)
    
    sorted_tbl <- sorted_tbl %>%
      dplyr::mutate(
        bh_critical_val = (rank_ascending/(num_tests * c_m_val))*q_val, 
        # indicator var: p-val is less than or equal to critical val
        p_val_smaller = ifelse(p_val <= bh_critical_val, "True", "False")
      )
  }
  
  
  # Part 3: Check -- Is there at least one p-val that is <= corresponding BH crit val? 
  
  if("True" %in% sorted_tbl$p_val_smaller) {
    
    # Find the rank of the largest p-value that is <= to its corresponding BH crit val
    k_val <- sorted_tbl %>% 
      dplyr::filter(p_val_smaller == "True") %>%
      dplyr::filter(rank_ascending == max(rank_ascending)) %>%
      pull(rank_ascending)
    
  } else { # If there is not at least one p-val that is <= corresponding BH crit val --> 
    
    # If none of the original p-vals are SS, then we don't need to adjust CIs for mult testing
    k_val <- num_tests 
    
  }
  
  
  # Part 4: Compute the adjusted alpha: case 1 (indep tests), case 2 (dep tests)
  
  if(indep_tests == TRUE) {
    
    # Compute adjusted alpha (given independent tests)
    adj_alpha <- (k_val/num_tests)*q_val  
    
  } else {
    
    # Compute adjusted alpha (given dependent tests)
    adj_alpha <- (k_val/(num_tests * c_m_val))*q_val 
  }
  
  
  # Compute the Z-score (assuming two-tailed, so divide adj alpha by 2)
  z_score <- stats::qnorm(1-(adj_alpha/2))
  
  return(z_score)
}



# testing values
by_2005_ci(p_vals_vec = c(.01, .01, .01), q_val = .05, indep_tests = TRUE)



