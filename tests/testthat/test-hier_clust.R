test_that("hier_clust works", {
    iris %>%
        dplyr::select(-Species) %>%
        hier_clust(k = 3)
})
