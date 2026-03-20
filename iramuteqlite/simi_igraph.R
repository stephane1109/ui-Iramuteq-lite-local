# Rôle du fichier: simi_igraph.R isole le rendu igraph du graphe de similitudes
# avec un scaling explicite des tailles (mots/sommets) et des arêtes.

simi_extraire_freq_vertices <- function(g, vertex_freq = NULL) {
  if (is.null(g) || !inherits(g, "igraph") || igraph::vcount(g) == 0) {
    return(numeric(0))
  }
  vf <- suppressWarnings(as.numeric(vertex_freq))
  if (length(vf) == igraph::vcount(g) && any(is.finite(vf))) {
    vf[!is.finite(vf)] <- 0
    return(pmax(vf, 0))
  }
  vsize <- suppressWarnings(as.numeric(igraph::V(g)$size))
  if (length(vsize) == igraph::vcount(g) && any(is.finite(vsize))) {
    vsize[!is.finite(vsize)] <- median(vsize[is.finite(vsize)], na.rm = TRUE)
    return(pmax(vsize, 0))
  }
  rep(1, igraph::vcount(g))
}

simi_tailles_sommets_igraph <- function(freq, min_out = 5, max_out = 58, power = 0.9) {
  f <- suppressWarnings(as.numeric(freq))
  if (!length(f) || all(!is.finite(f))) return(rep((min_out + max_out) / 2, length(f)))
  f[!is.finite(f)] <- 0
  f <- pmax(f, 0)^power
  as.numeric(normaliser_vecteur_simi(f, min_out, max_out))
}

simi_tailles_labels_igraph <- function(freq, min_out = 0.6, max_out = 3.3, power = 0.85) {
  f <- suppressWarnings(as.numeric(freq))
  if (!length(f) || all(!is.finite(f))) return(rep((min_out + max_out) / 2, length(f)))
  f[!is.finite(f)] <- 0
  f <- pmax(f, 0)^power
  as.numeric(normaliser_vecteur_simi(f, min_out, max_out))
}

simi_largeurs_aretes_igraph <- function(weight, min_out = 0.35, max_out = 4.2, cap_out = 5.2) {
  w <- suppressWarnings(as.numeric(weight))
  if (!length(w) || all(!is.finite(w))) return(rep(1, length(w)))
  w[!is.finite(w)] <- 0
  w <- pmax(w, 0)
  wmax <- max(w, na.rm = TRUE)
  if (!is.finite(wmax) || wmax <= 0) return(rep(1, length(w)))

  scaled <- if (wmax <= 1) {
    normaliser_vecteur_simi(w, min_out, max_out)
  } else {
    normaliser_vecteur_simi(log1p(w), min_out, max_out)
  }
  pmin(pmax(scaled, min_out), cap_out)
}

tracer_graphe_similitudes <- function(g,
                                     layout = NULL,
                                     edge_labels = TRUE,
                                     edge_width_by_index = TRUE,
                                     vertex_text_by_freq = FALSE,
                                     vertex_freq = NULL,
                                     main = "Graphe de similitude",
                                     communities = NULL,
                                     halo = FALSE,
                                     zoom = 1,
                                     info_text = NULL) {
  if (is.null(g) || !inherits(g, "igraph") || igraph::vcount(g) == 0) {
    plot.new()
    text(0.5, 0.5, "Aucun graphe de similitude.\nCliquez sur 'Paramétrer' puis 'Lancer l'analyse de similitudes'.", cex = 1.0)
    return(invisible(NULL))
  }

  lo <- layout
  if (is.null(lo) || !is.matrix(lo) || nrow(lo) != igraph::vcount(g)) {
    lo <- igraph::layout_with_fr(g)
  }

  freq <- simi_extraire_freq_vertices(g, vertex_freq = vertex_freq)
  vertex_size <- simi_tailles_sommets_igraph(freq, min_out = 5, max_out = 58, power = 0.9)

  vertex_label_cex <- 0.95
  if (isTRUE(vertex_text_by_freq)) {
    vertex_label_cex <- simi_tailles_labels_igraph(freq, min_out = 0.6, max_out = 3.3, power = 0.85)
  }

  if (isTRUE(edge_width_by_index)) {
    edge_width <- simi_largeurs_aretes_igraph(igraph::E(g)$weight, min_out = 0.35, max_out = 4.2, cap_out = 5.2)
  } else {
    edge_width <- rep(1, igraph::ecount(g))
  }

  zoom <- suppressWarnings(as.numeric(zoom))
  if (!is.finite(zoom) || is.na(zoom) || zoom <= 0) zoom <- 1
  edge_lab <- if (isTRUE(edge_labels)) round(igraph::E(g)$weight, 3) else NA

  vcol <- "#2C7FB8"
  mark_groups <- NULL
  mark_col <- NULL
  mark_border <- NULL
  if (!is.null(communities) && inherits(communities, "communities")) {
    memb <- as.integer(igraph::membership(communities))
    if (length(memb) == igraph::vcount(g) && any(is.finite(memb))) {
      ncom <- max(memb, na.rm = TRUE)
      pal <- grDevices::hcl.colors(ncom, palette = "Dark 3")
      idx <- pmax(1L, pmin(length(pal), memb))
      vcol <- pal[idx]

      if (isTRUE(halo)) {
        comm <- igraph::communities(communities)
        mark_groups <- lapply(seq_along(comm), function(i) {
          vids <- comm[[i]]
          if (is.null(names(vids))) {
            igraph::V(g)[vids]
          } else {
            igraph::V(g)[names(vids)]
          }
        })
        mark_col <- grDevices::adjustcolor(pal[seq_along(mark_groups)], alpha.f = 0.18)
        mark_border <- pal[seq_along(mark_groups)]
      }
    }
  }

  plot(
    g,
    layout = lo * zoom,
    main = main,
    vertex.label = igraph::V(g)$name,
    vertex.size = vertex_size,
    vertex.color = vcol,
    vertex.frame.color = "white",
    vertex.label.family = "sans",
    vertex.label.font = 1,
    vertex.label.cex = vertex_label_cex,
    vertex.label.color = "navy",
    edge.width = edge_width,
    edge.color = grDevices::adjustcolor("#303030", alpha.f = 0.82),
    edge.label = edge_lab,
    edge.label.cex = 0.72,
    edge.label.color = "navy",
    mark.groups = mark_groups,
    mark.col = mark_col,
    mark.border = mark_border
  )
  if (!is.null(info_text) && nzchar(info_text)) {
    mtext(info_text, side = 3, line = 0.3, cex = 0.82, col = "#455A64")
  }
}
