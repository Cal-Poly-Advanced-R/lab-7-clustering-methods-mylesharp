# Hierarchical clustering function without using hclust
hier_clust <- function(data) {
    n <- nrow(data)
    dist_matrix <- dist(data)  # Compute the distance matrix

    # Initialize cluster assignments
    clusters <- 1:n

    # Main loop for hierarchical clustering
    while (n > 1) {
        # Find the minimum distance between clusters
        min_dist <- Inf
        merge_idx <- c(0, 0)
        for (i in 1:(n-1)) {
            for (j in (i+1):n) {
                if (dist_matrix[i, j] < min_dist) {
                    min_dist <- dist_matrix[i, j]
                    merge_idx <- c(i, j)
                }
            }
        }

        # Merge the clusters with the minimum distance
        clusters[clusters == merge_idx[2]] <- merge_idx[1]

        # Update the distance matrix by merging the clusters
        dist_matrix <- dist_matrix[-merge_idx[2], -merge_idx[2]]
        dist_matrix <- rbind(dist_matrix, apply(dist_matrix, 2, min))
        dist_matrix <- cbind(dist_matrix, apply(dist_matrix, 1, min))

        # Remove the merged cluster
        clusters <- clusters - (clusters > merge_idx[2])
        n <- n - 1
    }

    return(clusters)
}




