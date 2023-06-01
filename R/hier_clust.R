# Hierarchical clustering function
hier_clust <- function(data) {
    dist_matrix <- dist(data)  # Compute the distance matrix

    hclust_result <- hclust(dist_matrix)  # Perform hierarchical clustering

    cut_height <- 2 * median(hclust_result$height)
    clusters <- cutree(hclust_result, h = cut_height)

    return(clusters)
}




