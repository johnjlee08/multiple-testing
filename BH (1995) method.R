# Code to implement the Benjamini and Hochberg (1995) procedure to adjust p-values for 
# multiple independent tests
# Author: John Lee 



# Load packages
library(tidyverse)


## Part 1: code to implement BH (1995) -- for mult testing given indep

bh_1995_adj <- function(p_vals_vec, q_val){
  
  num_tests <- length(p_vals_vec)
  
  # Set up the original tbl of p-vals (in the original order -- as inputted by user)
  original_tbl <- tibble(p_val = p_vals_vec) %>%
    dplyr::mutate(original_order = dplyr::row_number())
  
  # Set up the tbl w/ p-vals in ascending order (smallest at top)
  sorted_tbl <- original_tbl %>%
    # Sort in ascending order 
    dplyr::arrange(p_val) %>%
    # Create a new var denoting rank (ascending: rank 1 = smallest p-val)
    dplyr::mutate(
      rank_ascending = dplyr::row_number(),
      bh_critical_val = (rank_ascending/num_tests)*q_val, 
      
      # indicator: p-val is less than or equal to critical val
      p_val_smaller = ifelse(p_val <= bh_critical_val, "True", "False")
      
    )
  
  # Is there at least one p-val that is <= corresponding BH crit val? 
  
  if("True" %in% sorted_tbl$p_val_smaller) {
    
    # Find the rank of the largest p-value that is <= to its corresponding BH crit val
    k_val <- sorted_tbl %>%
      dplyr::filter(p_val_smaller == "True") %>%
      dplyr::filter(rank_ascending == max(rank_ascending)) %>%
      pull(rank_ascending)
    
    # Apply the decision rule: identify the original p-vals as SS or NS (based on BH 1995)
    final_tbl <- sorted_tbl %>%
      dplyr::mutate(
        
        bh_1995_result = ifelse(rank_ascending <= k_val, "sig.", "n.s.")
        
      )
  } else { # If there is not at least one p-val that is <= corresponding BH crit val --> 
    
    final_tbl <- sorted_tbl %>%
      dplyr::mutate(bh_1995_result = "n.s.")
  }
  
  # Resort per original order (entered by user) -- print results
  final_tbl %>% 
    dplyr::arrange(original_order) %>% 
    dplyr::select(p_val, bh_1995_result)
}



