#' Quantify topic occurence in ExperimentHub resources and combine package
#' related resources
#'
#' @param topics `character()` A vector of topics to query `ExperimentHub`
#'   resources with
#'
#' @param pkg_pattern `character(1)` A pattern to use for searching through
#'   packages that expose `ExperimentHub` resources related to `topics`
#'
#' @return The proportion of all `ExperimentHub` resources that match both the
#'   topics and package pattern
#'
#' @examples
#' et <- EHubTopicProp(c("cancer", "tumor"), "curated*")
#'
#' @export
EHubTopicProp <- function(topics, pkg_pattern) {
    eh <- ExperimentHub::ExperimentHub()
    ehlist <- lapply(topics, query, x = eh)
    ah_ids <- lapply(ehlist, function(x) {
        x$ah_id
    })
    ah_ids <- unique(unlist(ah_ids))

    pkgnames <- BiocManager::available(pkg_pattern)
    pkg_eh <- Filter(length, lapply(pkgnames, query, x = eh))
    pkg_ids <- lapply(pkg_eh, function(x) {
        x$ah_id
    })
    pkg_ids <- unique(unlist(ah_ids))

    res_ids <- unique(c(ah_ids, pkg_ids))
    length(res_ids) / length(eh$ah_id)
}
