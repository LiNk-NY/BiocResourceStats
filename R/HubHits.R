#' Quantify topic occurence in ExperimentHub resources and combine package
#' related resources
#'
#' @param topics `character()` A vector of topics to query `ExperimentHub`
#'   resources with
#'
#' @param pkg_pattern `character(1)` A pattern to use for searching through
#'   packages that expose `ExperimentHub` resources related to `topics`
#'
#' @param hub `character(1)` Either 'ExperimentHub' or 'AnnotationHub'
#'   indicating which hub to query resources from
#'
#' @return The proportion of all hub resources that match both the topics and
#'   package pattern
#'
#' @examples
#' et <- HubHits(
#'     c("cancer", "tumor"), "curated*", hub = "ExperimentHub"
#' )
#' # proportion
#' et[1] / et[2]
#'
#' @export
HubHits <- function(
    topics, pkg_pattern, hub = c("ExperimentHub", "AnnotationHub")
) {
    hub <- match.arg(hub)
    if (identical(hub, "ExperimentHub"))
        hub <- ExperimentHub::ExperimentHub()
    else
        hub <- AnnotationHub::AnnotationHub()

    hublist <- lapply(topics, AnnotationHub::query, x = hub)
    ah_ids <- lapply(hublist, function(x) {
        x$ah_id
    })
    ah_ids <- unlist(ah_ids)

    pkgnames <- BiocManager::available(pkg_pattern)
    pkg_hub <- Filter(length, lapply(pkgnames, AnnotationHub::query, x = hub))
    pkg_ids <- lapply(pkg_hub, function(x) {
        x$ah_id
    })
    pkg_ids <- unlist(pkg_ids)

    res_ids <- unique(c(ah_ids, pkg_ids))
    res <- c(n.hits = length(res_ids), total = length(hub$ah_id))
    class(res) <- "HubHits"
    res
}

prop.HubHits <- function(HubHits) {
    stopifnot(inherits(HubHits, "HubHits"))
    prop <- HubHits['n.hits'] / HubHits["total"]
    names(prop) <- "prop.hits"
    prop
}
