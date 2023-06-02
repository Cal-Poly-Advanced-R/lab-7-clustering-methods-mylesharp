#' Perform k means clustering on data
#'
#' @param dat The data
#' @parm k Number of clusters
#' @parm h
#'
#' @return returns Cluster Assignments
#'



cut_into_clusters <- function(join_steps, k, h = NULL) {
    join_steps <- join_steps[order(-join_steps[, 2]), , drop = FALSE]
    cluster_labels <- 1:k
    while (length(unique(cluster_labels)) > k) {
        next_step <- join_steps[1, ]
        indices <- as.integer(strsplit(next_step[1], "-")[[1]])
        cluster_labels[cluster_labels == indices[2]] <- indices[1]
        join_steps <- join_steps[-1, , drop = FALSE]
    }
    return(cluster_labels)
}

hier_clust <- function(dat, k, h = NULL) {
    join_steps <- matrix(ncol = 2)
    converged <- FALSE

    while (!converged) {
        dat_of_clusters <- list()

        # Assign observations to clusters
        cluster_assignments <- kmeans(dat, centers = k)$cluster

        # Separate data into clusters
        for (i in 1:k) {
            dat_of_clusters[[i]] <- dat[cluster_assignments == i, ]
        }

        # Find distances between clusters
        dist_matrix <- matrix(0, nrow = k, ncol = k)
        for (i in 1:(k - 1)) {
            for (j in (i + 1):k) {
                dist_matrix[i, j] <- dist_matrix[j, i] <- sqrt(sum((colMeans(dat_of_clusters[[i]]) - colMeans(dat_of_clusters[[j]]))^2))
            }
        }

        # Find smallest distance and join clusters
        min_dist <- min(dist_matrix[dist_matrix > 0])
        join_indices <- which(dist_matrix == min_dist, arr.ind = TRUE)
        joined_string <- paste0(join_indices[1, 1], "-", join_indices[1, 2])

        # Store the join step (joined_clusters, height)
        join_steps <- rbind(join_steps, c(joined_string, min_dist))

        # Merge the two closest clusters
        merged_cluster <- rbind(dat_of_clusters[[join_indices[1, 1]]], dat_of_clusters[[join_indices[1, 2]]])

        # Remove the separate clusters
        dat_of_clusters <- dat_of_clusters[-c(join_indices[1, 1], join_indices[1, 2])]

        # Add the merged cluster
        dat_of_clusters[[k]] <- merged_cluster

        k <- k - 1  # Update the number of clusters

        # Check convergence
        converged <- length(dat_of_clusters) == 1
    }

    cluster_assignments <- cut_into_clusters(join_steps, k = k, h = h)

    return(cluster_assignments)
}







