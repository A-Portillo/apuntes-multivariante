game_palette <- list(
  ink = "#14213D", muted = "#708090", grid = "#DDE3EA", paper = "#FBFAF7",
  coral = "#FF6B5E", teal = "#08A88A", yellow = "#F7C948", violet = "#705CF6"
)

plot_frame <- function(data, title = NULL) {
  xpad <- diff(range(data$x)) * 0.14
  ypad <- diff(range(data$y)) * 0.14
  graphics::par(
    mar = c(3.2, 3.2, 2.3, 1), bg = game_palette$paper,
    fg = game_palette$ink, col.axis = game_palette$muted,
    col.lab = game_palette$ink, family = "sans"
  )
  graphics::plot(
    data$x, data$y, type = "n", asp = 1, xlab = "variable 1", ylab = "variable 2",
    xlim = range(data$x) + c(-xpad, xpad), ylim = range(data$y) + c(-ypad, ypad),
    axes = FALSE
  )
  graphics::abline(h = pretty(data$y), v = pretty(data$x), col = game_palette$grid, lwd = 1)
  graphics::axis(1, lwd = 0, lwd.ticks = 1, col.ticks = game_palette$grid)
  graphics::axis(2, lwd = 0, lwd.ticks = 1, col.ticks = game_palette$grid, las = 1)
  if (!is.null(title)) graphics::title(main = title, adj = 0, cex.main = 1.05)
  graphics::points(data$x, data$y, pch = 21, cex = 1.05, lwd = 1.5,
                   bg = "#FFFFFF", col = game_palette$ink)
}

plot_mean_game <- function(data, point = NULL, reveal = FALSE) {
  plot_frame(data, "Encuentra el centro que minimiza la distancia cuadrática")
  if (!is.null(point)) {
    for (i in seq_len(nrow(data))) {
      graphics::segments(data$x[i], data$y[i], point[1], point[2],
                         col = grDevices::adjustcolor(game_palette$coral, 0.24), lwd = 1)
    }
    graphics::points(point[1], point[2], pch = 21, cex = 2.1, lwd = 2.4,
                     bg = game_palette$coral, col = "white")
  }
  if (reveal) {
    solution <- colMeans(data[, c("x", "y")])
    graphics::points(solution[1], solution[2], pch = 4, cex = 2.4, lwd = 3,
                     col = game_palette$teal)
  }
}

plot_kmeans_game <- function(data, centers = NULL, result = NULL, reveal = FALSE, k = 3L) {
  plot_frame(data, sprintf("Coloca exactamente %d centroides", as.integer(k)))
  if (!is.null(result) && !is.null(centers)) {
    cols <- c(game_palette$coral, game_palette$teal, game_palette$violet,
              game_palette$yellow, "#2F80ED")
    for (i in seq_len(nrow(data))) {
      k <- result$assignment[i]
      graphics::segments(data$x[i], data$y[i], centers[k, 1], centers[k, 2],
                         col = grDevices::adjustcolor(cols[k], 0.30), lwd = 1)
    }
  }
  if (!is.null(centers) && length(centers)) {
    if (reveal) {
      graphics::points(centers[, 1], centers[, 2], pch = 4, cex = 2.3, lwd = 3,
                       col = game_palette$teal)
    } else {
      graphics::points(centers[, 1], centers[, 2], pch = 23, cex = 2.3, lwd = 2.2,
                       bg = game_palette$yellow, col = game_palette$ink)
    }
  }
}

draw_infinite_line <- function(line, color = game_palette$coral) {
  usr <- graphics::par("usr")
  t <- c(-100, 100)
  line_points <- sweep(outer(t, line$direction), 2, line$origin, "+")
  graphics::lines(line_points[, 1], line_points[, 2], col = color, lwd = 3)
}

plot_pca_game <- function(data, clicks = NULL, result = NULL, reveal = FALSE) {
  plot_frame(data, "Dibuja la mejor recta de proyección")
  if (!is.null(result)) {
    for (i in seq_len(nrow(data))) {
      graphics::segments(data$x[i], data$y[i], result$projected[i, 1], result$projected[i, 2],
                         col = grDevices::adjustcolor(game_palette$violet, 0.35), lwd = 1.2)
    }
    graphics::points(result$projected[, 1], result$projected[, 2], pch = 16, cex = 0.65,
                     col = game_palette$violet)
    draw_infinite_line(result$line)
  }
  if (!is.null(clicks) && length(clicks)) {
    graphics::points(clicks[, 1], clicks[, 2], pch = 21, cex = 1.7,
                     bg = game_palette$coral, col = "white", lwd = 2)
  }
  if (reveal) {
    xy <- as.matrix(data[, c("x", "y")])
    center <- colMeans(xy)
    direction <- eigen(stats::cov(xy), symmetric = TRUE)$vectors[, 1]
    draw_infinite_line(list(origin = center, direction = direction), game_palette$teal)
  }
}

plot_regression_game <- function(data, clicks = NULL, result = NULL, reveal = FALSE) {
  plot_frame(data, "Predice y con una recta")
  if (!is.null(result)) {
    for (i in seq_len(nrow(data))) {
      graphics::segments(data$x[i], data$y[i], data$x[i], result$fitted[i],
                         col = grDevices::adjustcolor(game_palette$violet, 0.38), lwd = 1.3)
    }
    graphics::abline(a = result$intercept, b = result$slope, col = game_palette$coral, lwd = 3)
  }
  if (!is.null(clicks) && length(clicks)) {
    graphics::points(clicks[, 1], clicks[, 2], pch = 21, cex = 1.7,
                     bg = game_palette$coral, col = "white", lwd = 2)
  }
  if (reveal) {
    fit <- stats::lm(y ~ x, data = data)
    graphics::abline(fit, col = game_palette$teal, lwd = 3)
  }
}
