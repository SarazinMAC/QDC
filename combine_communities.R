#####################################################
##### Combine communities into adjacency matrix #####
#####################################################

### Note: Run select lines in QDC_stats_by_slice first

# Iterate over each second-order list in all_communities_combined
adj_matrices <- list()
for (i in 1:length(all_memberships_combined)) {
  # For each second-order list, create an adjacency matrix for each third-order list
  adj_matrices[[i]] <- lapply(all_memberships_combined[[i]], function(third_order_list) {
    # Create an adjacency matrix where each element is 1 if the corresponding actors are in the same community, and 0 otherwise
    adj_matrix <- outer(third_order_list, third_order_list, FUN = function(x, y) as.integer(x == y))
    return(adj_matrix)
  })
}
  
# Initialize an empty list to store the final result
final_matrices <- list()

# Get the number of matrices in each second-order list
num_matrices <- length(adj_matrices[[1]])

# For each index
for (i in 1:num_matrices) {
  # Extract the i-th matrix from each second-order list
  matrices_to_sum <- lapply(adj_matrices, function(second_order_list) second_order_list[[i]])
  
  # Sum the matrices
  sum_matrix <- Reduce('+', matrices_to_sum)
  sum_matrix <- sum_matrix/number_of_iterations
  
  # Add the summed matrix to the final result
  final_matrices[[i]] <- sum_matrix
}

# Now 'final_matrices' is a list of matrices, where each matrix is the sum of the matrices with the same index across the second-order lists
#TODO: automate naming of final_matrices

rio::export(list("1764" = final_matrices[[1]], "1765" = final_matrices[[2]]), paste0(export_path, "community_assignment.xlsx"), rowNames = TRUE)
