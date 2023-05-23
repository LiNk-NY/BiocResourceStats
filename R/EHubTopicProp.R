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
#' et <- HubTopicProp(
#'     c("cancer", "tumor"), "curated*", hubtype = "AnnotationHub"
#' )
#'
#' @export
HubTopicProp <- function(
    topics, pkg_pattern, hub = c("ExperimentHub", "AnnotationHub")
) {
    hubtype <- match.arg(hub)
    if (identical(hub, "ExperimentHub"))
        hub <- ExperimentHub::ExperimentHub()
    else
        hub <- AnnotationHub::AnnotationHub()

    hublist <- lapply(topics, AnnotationHub::query, x = hub)
    ah_ids <- lapply(hublist, function(x) {
        x$ah_id
    })
    ah_ids <- unique(unlist(ah_ids))

    pkgnames <- BiocManager::available(pkg_pattern)
    pkg_hub <- Filter(length, lapply(pkgnames, AnnotationHub::query, x = hub))
    pkg_ids <- lapply(pkg_hub, function(x) {
        x$ah_id
    })
    pkg_ids <- unique(unlist(pkg_ids))

    res_ids <- unique(c(ah_ids, pkg_ids))
    length(res_ids) / length(hub$ah_id)
}
