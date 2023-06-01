#' document
#'
#'
#'


# # Hierarchical clustering function without using hclust
# hier_clust <- function(data, k, h = NULL) {
#     n <- nrow(data)
#     dist_matrix <- dist(data)  # Compute the distance matrix
#
#     # Initialize cluster assignments
#     clusters <- 1:n
#
#     # Main loop for hierarchical clustering
#     while (n > 1) {
#         # Find the minimum distance between clusters
#         min_dist <- Inf
#         merge_idx <- c(0, 0)
#         for (i in 1:(n-1)) {
#             for (j in (i+1):n) {
#                 if (dist_matrix[i, j] < min_dist) {
#                     min_dist <- dist_matrix[i, j]
#                     merge_idx <- c(i, j)
#                 }
#             }
#         }
#
#         # Merge the clusters with the minimum distance
#         clusters[clusters == merge_idx[2]] <- merge_idx[1]
#
#         # Update the distance matrix by merging the clusters
#         dist_matrix <- dist_matrix[-merge_idx[2], -merge_idx[2]]
#         dist_matrix <- rbind(dist_matrix, apply(dist_matrix, 2, min))
#         dist_matrix <- cbind(dist_matrix, apply(dist_matrix, 1, min))
#
#         # Remove the merged cluster
#         clusters <- clusters - (clusters > merge_idx[2])
#         n <- n - 1
#     }
#
#     return(clusters)
# }



# #######
#
# hier_clust <- function(dat, k, h = NULL) {
#     join_steps <- c()
#
#     while(!converged) {
#         dat_of_clusters
#         ### one row per cluster
#         # find distances btwn clusters
#
#         # find smallest distance and join them
#
#         # join closest two
#         join_steps <- join_steps %>%
#             rbind(c(joined_strinf, height))
#
#         converged <-  nrow(dat_of_clusters) == 1
#
#     }
#
#     cluster_assignments <- join_steps %>%
#         cut_into_clusters(k = NULL, h = NULL)
# }


hier_clust <- function(dat, k, h = NULL) {
    join_steps <- c()
    converged <- FALSE

    while(!converged) {
        dat_of_clusters <- list()

        # Assign observations to clusters
        cluster_assignments <- kmeans(dat, centers = k)$cluster

        # Separate data into clusters
        for (i in 1:k) {
            dat_of_clusters[[i]] <- dat[cluster_assignments == i, ]
        }

        # Find distances between clusters
        dist_matrix <- matrix(0, nrow = k, ncol = k)
        for (i in 1:(k-1)) {
            for (j in (i+1):k) {
                dist_matrix[i, j] <- dist_matrix[j, i] <- sqrt(sum((colMeans(dat_of_clusters[[i]]) - colMeans(dat_of_clusters[[j]]))^2))
            }
        }

        # Find smallest distance and join clusters
        min_dist <- min(dist_matrix[dist_matrix > 0])
        join_indices <- which(dist_matrix == min_dist, arr.ind = TRUE)
        joined_string <- paste0(join_indices[1], "-", join_indices[2])

        # Store the join step (joined_clusters, height)
        join_steps <- rbind(join_steps, c(joined_string, min_dist))

        # Merge the two closest clusters
        merged_cluster <- c(dat_of_clusters[[join_indices[1], ]], dat_of_clusters[[join_indices[2], ]])

        # Remove the separate clusters
        dat_of_clusters <- dat_of_clusters[-c(join_indices[1], join_indices[2])]

        # Add the merged cluster
        dat_of_clusters[[k]] <- merged_cluster

        k <- k - 1  # Update the number of clusters

        # Check convergence
        converged <- length(dat_of_clusters) == 1

        if (is.null(h) && converged) {
            break
        } else if (!is.null(h) && min_dist >= h) {
            break
        }
    }

    cluster_assignments <- join_steps %>%
        cut_into_clusters(k = NULL, h = NULL)

    return(cluster_assignments)
}



