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

construire_graphe_similitudes <- function(dfm_obj,
                                         method = "cooc",
                                         seuil = NA_real_,
                                         max_tree = TRUE,
                                         top_terms = 40L,
                                         layout_type = "frutch",
                                         communities = FALSE,
                                         community_method = "edge_betweenness") {
  if (is.null(dfm_obj) || quanteda::ndoc(dfm_obj) < 2 || quanteda::nfeat(dfm_obj) < 2) {
    stop("DFM insuffisant pour construire un graphe de similitudes.")
  }

  mat_dfm <- as.matrix(dfm_obj)
  mat_bin <- ifelse(mat_dfm > 0, 1, 0)
  freq <- colSums(mat_bin)

  n_top <- suppressWarnings(as.integer(top_terms))
  if (!is.finite(n_top) || is.na(n_top) || n_top < 5) n_top <- 40L
  n_top <- min(n_top, ncol(mat_bin))

  ord <- order(freq, decreasing = TRUE)
  keep <- ord[seq_len(n_top)]
  mat_bin <- mat_bin[, keep, drop = FALSE]
  freq <- freq[keep]

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

  if (igraph::vcount(g) > 0) {
    vnames <- igraph::V(g)$name
    vfreq <- freq[match(vnames, names(freq))]
    if (!any(is.finite(vfreq))) vfreq <- rep(1, length(vnames))
    vfreq[!is.finite(vfreq)] <- median(vfreq[is.finite(vfreq)], na.rm = TRUE)

    igraph::V(g)$size <- as.numeric(normaliser_vecteur_simi(vfreq, 8, 24))
    if (igraph::ecount(g) > 0) {
      igraph::E(g)$width <- as.numeric(normaliser_vecteur_simi(igraph::E(g)$weight, 1, 6))
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
    communities = com
  )
}

tracer_graphe_similitudes <- function(g, layout = NULL, edge_labels = TRUE, main = "Graphe de similitude", communities = NULL, halo = FALSE) {
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

  edge_width <- igraph::E(g)$width
  if (is.null(edge_width)) edge_width <- 1

  edge_lab <- if (isTRUE(edge_labels)) round(igraph::E(g)$weight, 3) else NA

  vcol <- "#2C7FB8"
  mark_groups <- NULL
  if (!is.null(communities) && inherits(communities, "communities")) {
    memb <- igraph::membership(communities)
    memb <- as.integer(memb)
    if (length(memb) == igraph::vcount(g) && any(is.finite(memb))) {
      pal <- grDevices::rainbow(max(memb, na.rm = TRUE))
      idx <- pmax(1L, pmin(length(pal), memb))
      vcol <- pal[idx]
    }
    if (isTRUE(halo)) {
      mark_groups <- igraph::communities(communities)
    }
  }

  plot(
    g,
    layout = lo,
    vertex.label = vertex_labels,
    vertex.label.cex = 0.8,
    vertex.size = vertex_size,
    vertex.color = vcol,
    vertex.frame.color = "#1f4f7a",
    edge.width = edge_width,
    edge.color = grDevices::adjustcolor("#4D4D4D", alpha.f = 0.6),
    edge.label = edge_lab,
    edge.label.cex = 0.7,
    mark.groups = mark_groups,
    main = main
  )
}
