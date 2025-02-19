#
# Requires an adj_list made from EpiModel:::get_adj_list
#

get_subnet <- function(adj_list, nodes) {
  n_nodes <- length(adj_list)
  subnet <- nodes
  while (length(nodes) > 0 && length(subnet) < n_nodes) {
    nodes <- unlist(adj_list[nodes])
    nodes <- setdiff(nodes, subnet)
    subnet <- c(subnet, nodes)
  }
  subnet
}

get_all_subnets <- function(adj_list) {
  subnets_ids <- rep(0, length(adj_list))
  subnets_lengths <- c()
  current_subnet <- 1
  for (i in seq_along(subnets_ids)) {
    if (subnets_ids[i] > 0) next
    new_subnet <- get_subnet(adj_list, i)
    subnets_lengths <- c(subnets_lengths, length(new_subnet))
    subnets_ids[new_subnet] <- current_subnet
    current_subnet <- current_subnet + 1
  }
  list(
    ids = subnets_ids,
    lengths = subnets_lengths
  )
}
