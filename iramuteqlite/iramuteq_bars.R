# Rôle du fichier: rendu du dendrogramme CHD style IRaMuTeQ (barres à droite).

tracer_dendrogramme_iramuteq_bars <- function(edges_df,
                                              node_xy,
                                              class_tip_keys,
                                              class_by_tip,
                                              pct_par_classe,
                                              max_depth,
                                              x_vals,
                                              .palette_classes_iramuteq,
                                              .draw_tree_edge) {
  par(mar = c(1, 1, 3, 1))

  tip_keys <- class_tip_keys
  tip_pos <- vapply(tip_keys, function(k) {
    xy <- node_xy[[k]]
    if (is.null(xy)) return(NA_real_)
    as.numeric(xy[["x"]])
  }, numeric(1))
  ok_tip <- is.finite(tip_pos)
  tip_keys <- tip_keys[ok_tip]

  classes_tip <- as.integer(class_by_tip[tip_keys])
  pct_tip <- vapply(classes_tip, function(cl) {
    if (is.null(pct_par_classe)) return(NA_real_)
    suppressWarnings(as.numeric(pct_par_classe[[as.character(cl)]]))
  }, numeric(1))
  if (!length(pct_tip) || all(!is.finite(pct_tip))) {
    pct_tip <- rep(100 / max(1, length(tip_keys)), length(tip_keys))
  }
  pct_tip[!is.finite(pct_tip) | pct_tip < 0] <- 0

  cols_map <- .palette_classes_iramuteq(classes_tip)
  tip_cols <- if (length(cols_map)) unname(cols_map[as.character(classes_tip)]) else rep("#7aa6ff", length(classes_tip))
  tip_cols[is.na(tip_cols) | !nzchar(tip_cols)] <- "#7aa6ff"

  tree_xmax <- max_depth + 0.35
  bar_left <- tree_xmax + 1.2
  bar_max <- 4.8
  x_right <- bar_left + bar_max + 0.8

  plot(0, 0,
       type = "n",
       xlim = c(-0.5, x_right),
       ylim = c(min(x_vals) - 0.5, max(x_vals) + 0.5),
       axes = FALSE,
       xlab = "", ylab = "",
       main = "Dendrogramme CHD"
  )

  for (i in seq_len(nrow(edges_df))) {
    p_key <- as.character(edges_df$parent[[i]])
    c_key <- as.character(edges_df$child[[i]])
    p_xy <- node_xy[[p_key]]
    c_xy <- node_xy[[c_key]]
    if (is.null(p_xy) || is.null(c_xy)) next
    .draw_tree_edge(
      x1 = p_xy[["y"]], y1 = p_xy[["x"]],
      x2 = c_xy[["y"]], y2 = c_xy[["x"]],
      mode = "horizontal_tree",
      col = "#707070",
      lwd = 2.4,
      xpd = TRUE
    )
  }

  for (i in seq_along(tip_keys)) {
    tip <- tip_keys[[i]]
    xy <- node_xy[[tip]]
    if (is.null(xy)) next

    cl <- classes_tip[[i]]
    pct <- pct_tip[[i]]
    col_bar <- tip_cols[[i]]

    classe_txt <- paste0("classe ", cl)
    text(tree_xmax + 0.08, xy[["x"]], labels = classe_txt, cex = 1.1, pos = 4, xpd = TRUE,
         col = col_bar, font = 3)

    width <- bar_max * (pct / 100)
    rect(
      xleft = bar_left,
      ybottom = xy[["x"]] - 0.42,
      xright = bar_left + width,
      ytop = xy[["x"]] + 0.42,
      col = grDevices::adjustcolor(col_bar, alpha.f = 0.95),
      border = "#6f6f6f",
      lwd = 1.2,
      xpd = TRUE
    )

    pct_lab <- paste0(format(round(pct, 1), nsmall = 1), " %")
    text(bar_left + width - 0.12, xy[["x"]], labels = pct_lab, pos = 2, cex = 0.95, xpd = TRUE, col = "#1f1f1f")
  }

  invisible(NULL)
}
