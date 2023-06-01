
k_means <- function(dat, k, pca = FALSE) {
        if (pca) {
        pc <- princomp(dat)
        dat <- as.matrix(pc$scores[, 1:2])
        }


    set.seed(123)
    n <- nrow(dat)
    old_centers <- dat[sample(1:n, k), ]

    cluster <- rep(NA, n)
    tss <- 0

    while (TRUE) {
        for (i in 1:n) {
            euclid <- sqrt(rowSums((dat[i, ] - old_centers)^2))
            cluster[i] <- which.min(euclid)
        }

        new_centers <- tapply(dat, cluster, colMeans)

        #convergene
        if (identical(old_centers, new_centers)) {
            break
        }

        tss <- sum(sapply(1:k, function(j) sum((dat[cluster == j, ] - new_centers[j, ])^2)))

        old_centers <- new_centers
    }

    result <- list(cluster, tss)
    return(result)
}
