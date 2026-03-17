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
  label_x <- tree_xmax + 1.45
  bar_left <- tree_xmax + 2.15
  bar_max <- 5.4
  x_right <- bar_left + bar_max + 1.0

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
    text(label_x, xy[["x"]], labels = classe_txt, cex = 1.45, pos = 2, xpd = TRUE,
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
    txt_x <- if (width >= 1.15) {
      bar_left + width - 0.15
    } else {
      bar_left + width + 0.14
    }
    txt_pos <- if (width >= 1.15) 2 else 4
    text(txt_x, xy[["x"]], labels = pct_lab, pos = txt_pos, cex = 1.2, xpd = TRUE, col = "#1f1f1f")
  }

  invisible(NULL)
}

tracer_dendrogramme_iramuteq_bars_hclust <- function(hc,
                                                     classes = NULL,
                                                     main = "Dendrogramme CHD") {
  if (is.null(hc) || !inherits(hc, "hclust")) return(FALSE)

  .palette_classes <- function(ids) {
    ids <- as.integer(ids)
    ids <- ids[is.finite(ids)]
    if (!length(ids)) return(character(0))
    palette_base <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#00BA38", "#619CFF", "#F564E3", "#B79F00")
    uniq <- sort(unique(ids))
    stats::setNames(rep_len(palette_base, length(uniq)), as.character(uniq))
  }

  pct_par_classe <- NULL
  if (!is.null(classes)) {
    classes_int <- suppressWarnings(as.integer(classes))
    classes_int <- classes_int[is.finite(classes_int) & classes_int > 0]
    if (length(classes_int)) pct_par_classe <- prop.table(table(classes_int)) * 100
  }

  ord <- hc$order
  ord <- ord[is.finite(ord) & ord >= 1 & ord <= length(hc$labels)]
  if (!length(ord)) return(FALSE)

  labels_ord <- hc$labels[ord]
  classes_tip <- suppressWarnings(as.integer(sub("^\\s*Classe\\s+([0-9]+).*$", "\\1", labels_ord, perl = TRUE)))
  classes_tip[!is.finite(classes_tip)] <- seq_along(classes_tip)[!is.finite(classes_tip)]

  pct_tip <- vapply(classes_tip, function(cl) {
    if (is.null(pct_par_classe)) return(NA_real_)
    suppressWarnings(as.numeric(pct_par_classe[[as.character(cl)]]))
  }, numeric(1))
  if (!length(pct_tip) || all(!is.finite(pct_tip))) pct_tip <- rep(100 / max(1, length(classes_tip)), length(classes_tip))
  pct_tip[!is.finite(pct_tip) | pct_tip < 0] <- 0

  cols_map <- .palette_classes(classes_tip)
  tip_cols <- if (length(cols_map)) unname(cols_map[as.character(classes_tip)]) else rep("#7aa6ff", length(classes_tip))
  tip_cols[is.na(tip_cols) | !nzchar(tip_cols)] <- "#7aa6ff"

  old_mar <- par("mar")
  old_xpd <- par("xpd")
  on.exit(par(mar = old_mar, xpd = old_xpd), add = TRUE)

  par(mar = c(1, 1, 3, 10), xpd = NA)

  plot(
    hc,
    horiz = TRUE,
    axes = FALSE,
    ann = FALSE,
    labels = FALSE,
    hang = -1,
    frame.plot = FALSE,
    main = main
  )

  usr <- par("usr")
  span_x <- abs(usr[[1]] - usr[[2]])
  if (!is.finite(span_x) || span_x <= 0) span_x <- 1

  y_pos <- seq_along(classes_tip)
  label_x <- usr[[2]] - span_x * 0.12
  bar_left <- usr[[2]] - span_x * 0.30
  bar_max <- span_x * 0.22

  for (i in seq_along(classes_tip)) {
    cl <- classes_tip[[i]]
    pct <- pct_tip[[i]]
    col_bar <- tip_cols[[i]]

    text(label_x, y_pos[[i]], labels = paste0("classe ", cl), cex = 1.1, pos = 2, col = col_bar, font = 3, xpd = TRUE)

    width <- bar_max * (pct / 100)
    rect(bar_left, y_pos[[i]] - 0.42, bar_left + width, y_pos[[i]] + 0.42,
         col = grDevices::adjustcolor(col_bar, alpha.f = 0.95), border = "#6f6f6f", lwd = 1.2, xpd = TRUE)

    pct_lab <- paste0(format(round(pct, 1), nsmall = 1), " %")
    txt_x <- if (width >= bar_max * 0.24) bar_left + width - 0.08 else bar_left + width + 0.10
    txt_pos <- if (width >= bar_max * 0.24) 2 else 4
    text(txt_x, y_pos[[i]], labels = pct_lab, pos = txt_pos, cex = 1.0, col = "#1f1f1f", xpd = TRUE)
  }

  TRUE
}

