# Rôle du fichier: rendu dendrogramme CHD via ggdendro/ggplot2.

tracer_dendrogramme_ggdendro <- function(hc,
                                          orientation = c("vertical", "horizontal"),
                                          main = "Dendrogramme CHD (ggdendro)") {
  orientation <- match.arg(orientation)
  if (is.null(hc) || !inherits(hc, "hclust")) return(FALSE)
  if (!requireNamespace("ggdendro", quietly = TRUE)) return(FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)

  tryCatch({
    ddata <- ggdendro::dendro_data(as.dendrogram(hc), type = "rectangle")
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = ggdendro::segment(ddata),
        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
        linewidth = 0.6
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::labs(title = main, x = NULL, y = NULL) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )

    if (identical(orientation, "horizontal")) {
      p <- p + ggplot2::coord_flip()
    }

    print(p)
    TRUE
  }, error = function(e) FALSE)
}
