#' Perform k means clustering on data
#'
#' @param dat The data
#' @parm k Number of clusters
#'
#' @return A vector of cluster levels in order of data set rows
#'
#' @import dplyr Rfast
#'


k_means <- function(dat, k, pca = FALSE) {

    #stop if k is not a positive number
    if(!is.numeric(k)||k<=0) {
        stop("k must be a positive number")
    }


    for (col in names(dat)) {
        if (is.factor(dat[[col]]) || is.character(dat[[col]])) {
            dat <- dat %>% select(-col)
            }
    }


    if (pca) {
        pca_data <- princomp(data, cor = TRUE)
        data <- predict(pca_data)
    }


    dat_m <- dat %>%
        as.matrix()

    olds_centriods <- dat_m %>%
        sample_n(k) %>%
        as.matrix()


    while(TRUE) {
        dists <- dist(dat_m, method = "euclidean")
        cluster_assignments <- apply(dists, 1, which.min)

        new_centroids <- dat %>%
            mutate(clust = cluster_assignments) %>%
            group_by(clust) %>%
            summarize_all(mean) %>%
            select(-clust) %>%
            as.matrix()


        if (all(olds_centroids == new_centroids)) {
            break
        }

        olds_centriods <- new_centroids
    }


    return(cluster_assignments)

}


