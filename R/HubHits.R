#' Quantify topic occurence in *Hub resources and combine package related
#' resources
#'
#' @param x `HubHits` An instance of `HubHits` as returned by the eponymous
#'   constructor function
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
#' @return
#' * HubHits: A `HubHits` instance
#' * prop.HubHits: The proportion of search hits to all resources in the
#'   specified hub
#'
#' @examples
#' et <- HubHits(c("cancer", "tumor"), "curated*", hub = "ExperimentHub")
#'
#' @export
HubHits <- function(
    topics, pkg_pattern, hub = c("ExperimentHub", "AnnotationHub")
) {
    hubname <- match.arg(hub)
    if (identical(hubname, "ExperimentHub"))
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
    attr(res, "Hub") <- hubname
    attr(res, "topics") <- topics
    attr(res, "pkg_pattern") <- pkg_pattern
    res
}

#' @describeIn HubHits
#'
#' @examples
#' # proportion of hits in all *Hub
#' prop.HubHits(et)
#'
#' @export
prop.HubHits <- function(x) {
    stopifnot(inherits(x, "HubHits"))
    prop <- x['n.hits'] / x["total"]
    names(prop) <- "prop.hits"
    prop
}

#' @describeIn HubHits
#'
#' @examples
#' # print HubHits
#' print.HubHits(et)
#' @export
print.HubHits <- function(x) {
    topics <- attr(x, "topics")
    pattern <- attr(x, "pkg_pattern")
    cat(
        "HubHits(c(", paste(sQuote(topics, FALSE), collapse = ", "), "), ",
        sQuote(pattern, FALSE), ")\n", sep = ""
    )
    cat(
        x['n.hits'], " / ", x['total'], "=", prop.HubHits(x)
    )
}
