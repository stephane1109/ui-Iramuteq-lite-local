# Rôle du fichier: rendu dendrogramme CHD via le package dendextend.

tracer_dendrogramme_dendextend <- function(hc,
                                            orientation = c("vertical", "horizontal"),
                                            main = "Dendrogramme CHD (dendextend)") {
  orientation <- match.arg(orientation)
  if (is.null(hc) || !inherits(hc, "hclust")) return(FALSE)
  if (!requireNamespace("dendextend", quietly = TRUE)) return(FALSE)

  dnd <- tryCatch(as.dendrogram(hc), error = function(e) NULL)
  if (is.null(dnd)) return(FALSE)

  tryCatch({
    dnd <- dendextend::set(dnd, "branches_lwd", 1.5)
    dnd <- dendextend::set(dnd, "labels_cex", 0.78)
    if (identical(orientation, "horizontal")) {
      plot(dnd, horiz = TRUE, main = main, xlab = "", ylab = "")
    } else {
      plot(dnd, main = main, xlab = "", ylab = "")
    }
    TRUE
  }, error = function(e) FALSE)
}
