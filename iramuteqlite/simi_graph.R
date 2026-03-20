# Rôle du fichier: simi_graph.R centralise la construction et le tracé du graphe de similitudes.


calculer_communautes_simi <- function(g, method = "edge_betweenness") {
  if (is.null(g) || !inherits(g, "igraph") || igraph::vcount(g) < 2 || igraph::ecount(g) < 1) {
    return(NULL)
  }

  tryCatch({
    if (identical(method, "fast_greedy")) {
      igraph::cluster_fast_greedy(g, weights = igraph::E(g)$weight)
    } else if (identical(method, "label_propagation")) {
      igraph::cluster_label_prop(g, weights = igraph::E(g)$weight)
    } else if (identical(method, "leading_eigen")) {
      igraph::cluster_leading_eigen(g, weights = igraph::E(g)$weight)
    } else if (identical(method, "multilevel")) {
      igraph::cluster_louvain(g, weights = igraph::E(g)$weight)
    } else if (identical(method, "walktrap")) {
      igraph::cluster_walktrap(g, weights = igraph::E(g)$weight)
    } else {
      igraph::cluster_edge_betweenness(g, weights = 1 / pmax(igraph::E(g)$weight, 1e-9))
    }
  }, error = function(e) NULL)
}

normaliser_vecteur_simi <- function(x, min_out = 0, max_out = 1) {
  x <- as.numeric(x)
  if (!length(x) || all(!is.finite(x))) return(rep((min_out + max_out) / 2, length(x)))

  x_ok <- x[is.finite(x)]
  if (!length(x_ok)) return(rep((min_out + max_out) / 2, length(x)))

  xmin <- min(x_ok)
  xmax <- max(x_ok)
  if (!is.finite(xmin) || !is.finite(xmax) || identical(xmin, xmax)) {
    return(rep((min_out + max_out) / 2, length(x)))
  }

  min_out + ((x - xmin) / (xmax - xmin)) * (max_out - min_out)
}

calculer_largeurs_aretes_simi <- function(w, max_out = 12, min_out = 1, cap_out = 30) {
  w <- suppressWarnings(as.numeric(w))
  if (!length(w) || all(!is.finite(w))) return(rep(1, length(w)))
  w[!is.finite(w)] <- 0
  w <- pmax(w, 0)
  wmax <- max(w, na.rm = TRUE)
  if (!is.finite(wmax) || wmax <= 0) return(rep(1, length(w)))
  widths <- if (wmax <= 1) {
    # Métriques bornées (jaccard/binom): on amplifie pour rendre visible.
    w * max_out
  } else {
    # Cooccurrence (fréquences): largeur directement proportionnelle à la fréquence.
    w
  }
  pmin(pmax(widths, min_out), cap_out)
}

supprimer_sommets_isoles_simi <- function(g) {
  if (is.null(g) || !inherits(g, "igraph") || igraph::vcount(g) == 0) {
    return(g)
  }
  deg <- igraph::degree(g)
  if (!length(deg) || !any(deg == 0)) {
    return(g)
  }
  igraph::delete_vertices(g, which(deg == 0))
}

construire_graphe_similitudes <- function(dfm_obj,
                                         method = "cooc",
                                         seuil = NA_real_,
                                         max_tree = TRUE,
                                         top_terms = 40L,
                                         selected_terms = NULL,
                                         layout_type = "frutch",
                                         communities = FALSE,
                                         community_method = "edge_betweenness") {
  if (is.null(dfm_obj) || quanteda::ndoc(dfm_obj) < 2 || quanteda::nfeat(dfm_obj) < 2) {
    stop("DFM insuffisant pour construire un graphe de similitudes.")
  }

  mat_dfm <- as.matrix(dfm_obj)
  mat_bin <- ifelse(mat_dfm > 0, 1, 0)
  # Fréquences d'occurrence (et non présence/absence par document)
  # pour piloter la taille des sommets et des labels.
  freq <- colSums(mat_dfm)

  n_top <- suppressWarnings(as.integer(top_terms))
  if (length(n_top) != 1L || !is.finite(n_top) || is.na(n_top) || n_top < 5) n_top <- 40L
  n_top <- min(n_top, ncol(mat_bin))

  selected_terms <- unique(as.character(selected_terms))
  selected_terms <- selected_terms[!is.na(selected_terms)]
  selected_terms <- selected_terms[nzchar(selected_terms)]
  selected_terms <- intersect(selected_terms, colnames(mat_bin))

  if (length(selected_terms) > 0) {
    keep <- match(selected_terms, colnames(mat_bin))
    keep <- keep[is.finite(keep) & !is.na(keep)]
    mat_bin <- mat_bin[, keep, drop = FALSE]
    freq <- freq[keep]

    if (length(freq) > 1) {
      ord_sel <- order(freq, decreasing = TRUE)
      mat_bin <- mat_bin[, ord_sel, drop = FALSE]
      freq <- freq[ord_sel]
    }
  } else {
    ord <- order(freq, decreasing = TRUE)
    keep <- ord[seq_len(n_top)]
    mat_bin <- mat_bin[, keep, drop = FALSE]
    freq <- freq[keep]
  }

  method <- if (is.null(method) || !nzchar(method)) "cooc" else method

  cooc <- t(mat_bin) %*% mat_bin
  diag(cooc) <- 0

  if (identical(method, "jaccard")) {
    denom <- outer(colSums(mat_bin), colSums(mat_bin), "+") - cooc
    sim <- ifelse(denom > 0, cooc / denom, 0)
    diag(sim) <- 0
  } else if (identical(method, "binom")) {
    if (exists("binom.sim", mode = "function", inherits = TRUE)) {
      pmat <- tryCatch(binom.sim(mat_bin), error = function(e) NULL)
      if (!is.null(pmat)) {
        sim <- 1 - pmat
        sim[!is.finite(sim)] <- 0
        diag(sim) <- 0
      } else {
        sim <- cooc / max(cooc, na.rm = TRUE)
      }
    } else {
      sim <- cooc / max(cooc, na.rm = TRUE)
    }
  } else {
    sim <- cooc
  }

  sim[!is.finite(sim)] <- 0
  diag(sim) <- 0

  seuil_val <- suppressWarnings(as.numeric(seuil))
  if (is.finite(seuil_val) && !is.na(seuil_val)) {
    sim[sim <= seuil_val] <- 0
  } else {
    seuil_val <- NA_real_
  }

  g <- igraph::graph_from_adjacency_matrix(sim, mode = "undirected", weighted = TRUE, diag = FALSE)

  if (igraph::ecount(g) > 0 && isTRUE(max_tree) && igraph::ecount(g) > 1) {
    w <- igraph::E(g)$weight
    igraph::E(g)$weight <- 1 / pmax(w, 1e-9)
    g <- igraph::mst(g)
    igraph::E(g)$weight <- 1 / pmax(igraph::E(g)$weight, 1e-9)
  }
  g <- supprimer_sommets_isoles_simi(g)

  if (igraph::vcount(g) > 0) {
    vnames <- igraph::V(g)$name
    vfreq <- freq[match(vnames, names(freq))]
    if (!any(is.finite(vfreq))) vfreq <- rep(1, length(vnames))
    vfreq[!is.finite(vfreq)] <- median(vfreq[is.finite(vfreq)], na.rm = TRUE)

    igraph::V(g)$size <- as.numeric(normaliser_vecteur_simi(vfreq, 8, 24))
    if (igraph::ecount(g) > 0) {
      igraph::E(g)$width <- as.numeric(calculer_largeurs_aretes_simi(igraph::E(g)$weight, max_out = 12, min_out = 1, cap_out = 30))
    }
  } else {
    vfreq <- numeric(0)
  }

  lo <- switch(
    layout_type,
    frutch = igraph::layout_with_fr(g),
    kawa = igraph::layout_with_kk(g),
    circle = igraph::layout_in_circle(g),
    random = igraph::layout_on_grid(g),
    spirale = igraph::layout_with_fr(g),
    igraph::layout_with_fr(g)
  )

  com <- NULL
  if (isTRUE(communities)) {
    com <- calculer_communautes_simi(g, method = community_method)
  }

  list(
    graph = g,
    layout = lo,
    vertex_freq = vfreq,
    method = method,
    seuil = seuil_val,
    communities = com,
    n_terms_used = ncol(mat_bin),
    n_terms_total = ncol(mat_dfm),
    top_terms_requested = n_top
  )
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

  vertex_labels <- igraph::V(g)$name
  vertex_size <- igraph::V(g)$size
  if (is.null(vertex_size)) vertex_size <- 8
  vertex_label_cex <- 0.95
  if (isTRUE(vertex_text_by_freq)) {
    vf <- suppressWarnings(as.numeric(vertex_freq))
    if (!length(vf) || length(vf) != igraph::vcount(g)) {
      vf <- suppressWarnings(as.numeric(vertex_size))
    }
    if (length(vf) == igraph::vcount(g)) {
      vertex_label_cex <- as.numeric(normaliser_vecteur_simi(vf, 0.75, 1.5))
    }
  }

  edge_width <- igraph::E(g)$width
  if (isTRUE(edge_width_by_index)) {
    edge_weight <- suppressWarnings(as.numeric(igraph::E(g)$weight))
    if (length(edge_weight) == igraph::ecount(g) && any(is.finite(edge_weight))) {
      edge_width <- as.numeric(calculer_largeurs_aretes_simi(edge_weight, max_out = 12, min_out = 1, cap_out = 30))
    } else if (is.null(edge_width) || length(edge_width) != igraph::ecount(g)) {
      edge_width <- 1
    }
  } else {
    edge_width <- 1
  }

  zoom <- suppressWarnings(as.numeric(zoom))
  if (!is.finite(zoom) || is.na(zoom) || zoom <= 0) zoom <- 1

  edge_lab <- if (isTRUE(edge_labels)) round(igraph::E(g)$weight, 3) else NA

  vcol <- "#2C7FB8"
  mark_groups <- NULL
  mark_col <- NULL
  mark_border <- NULL
  if (!is.null(communities) && inherits(communities, "communities")) {
    memb <- igraph::membership(communities)
    memb <- as.integer(memb)
    if (length(memb) == igraph::vcount(g) && any(is.finite(memb))) {
      ncom <- max(memb, na.rm = TRUE)
      pal <- grDevices::hcl.colors(ncom, palette = "Dark 3")
      idx <- pmax(1L, pmin(length(pal), memb))
      vcol <- pal[idx]

      if (isTRUE(halo)) {
        mark_groups <- igraph::groups(communities)
        mark_col <- grDevices::adjustcolor(pal, alpha.f = 0.15)
        mark_border <- grDevices::adjustcolor(pal, alpha.f = 0.85)
      }
    }
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(1.2, 1.2, 3.2, 1.2))

  lo_mat <- as.matrix(lo)
  if (ncol(lo_mat) < 2) {
    lo_mat <- cbind(lo_mat, rep(0, nrow(lo_mat)))
  }
  lo_plot <- lo_mat[, 1:2, drop = FALSE]
  lo_plot <- igraph::norm_coords(lo_plot, xmin = -1, xmax = 1, ymin = -1, ymax = 1)

  xlim_use <- c(-1 / zoom, 1 / zoom)
  ylim_use <- c(-1 / zoom, 1 / zoom)

  plot(
    g,
    layout = lo_plot,
    vertex.label = vertex_labels,
    vertex.label.cex = vertex_label_cex,
    vertex.size = vertex_size,
    vertex.color = vcol,
    vertex.frame.color = "#1f4f7a",
    edge.width = edge_width,
    edge.color = grDevices::adjustcolor("#303030", alpha.f = 0.85),
    edge.label = edge_lab,
    edge.label.cex = 0.75,
    mark.groups = mark_groups,
    mark.col = mark_col,
    mark.border = mark_border,
    main = main,
    xlim = xlim_use,
    ylim = ylim_use
  )

  if (!is.null(info_text) && nzchar(as.character(info_text))) {
    graphics::mtext(as.character(info_text), side = 3, line = 0.2, cex = 0.85, col = "#37474F")
  }
}
