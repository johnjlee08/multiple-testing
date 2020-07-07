# Code to implement the Benjamini and Yekutieli (2001) procedure to adjust p-values for  
# multiple dependent tests
# Author: John Lee 


# Load packages
library(tidyverse)



by_2001_adj <- function(p_vals_vec, q_val){
  
  # Part 1: compute the C(m) value (adjusts for dependence under arbitrary conditions)
  
  num_tests <- length(p_vals_vec)
  
  c_m_vec <- vector(mode = "double", length = num_tests)
  
  for (i in 1:num_tests) {
    
    c_m_vec[i] <- (1/i)
    
  }
  
  c_m_val <- sum(c_m_vec)
  
  # Part 2 - Compute the BY crit vals ----------------------------------------------
  
  # Set up the original tbl of p-vals (in the original order -- as inputted BY user)
  original_tbl <- tibble(p_val = p_vals_vec) %>%
    dplyr::mutate(original_order = dplyr::row_number())
  
  # Set up the tbl w/ p-vals in ascending order (smallest at top)
  sorted_tbl <- original_tbl %>%
    # Sort in ascending order 
    dplyr::arrange(p_val) %>%
    # Create a new var denoting rank (ascending: rank 1 = smallest p-val)
    dplyr::mutate(
      rank_ascending = dplyr::row_number(),
      by_critical_val = (rank_ascending/(num_tests * c_m_val))*q_val, 
      
      # indicator: p-val is less than or equal to critical val
      p_val_smaller = ifelse(p_val <= by_critical_val, "True", "False")
      
    )
  
  # Part 3: implement decision rule re: SS -------------------------------------
  
  # Are there at least one p-val that is <= corresponding BY crit val? 
  
  if("True" %in% sorted_tbl$p_val_smaller) {
    
    # Find the rank of the largest p-value that is <= to its corresponding BY crit val
    k_val <- sorted_tbl %>%
      dplyr::filter(p_val_smaller == "True") %>%
      dplyr::filter(rank_ascending == max(rank_ascending)) %>%
      pull(rank_ascending)
    
    # Apply the decision rule: identify the original p-vals as SS or NS 
    final_tbl <- sorted_tbl %>%
      dplyr::mutate(
        
        by_2001_result = ifelse(rank_ascending <= k_val, "sig.", "n.s.")
        
      )
  } else { # If there is not at least one p-val that is <= corresponding BY crit val --> 
    
    final_tbl <- sorted_tbl %>%
      dplyr::mutate(by_2001_result = "n.s.")
  }
  
  # Resort per original order (entered BY user) -- print results
  final_tbl %>% 
    dplyr::arrange(original_order) %>% 
    dplyr::select(p_val, by_2001_result)
}



