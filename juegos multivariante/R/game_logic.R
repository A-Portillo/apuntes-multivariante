clamp <- function(x, lower = 0, upper = 1000) {
  pmax(lower, pmin(upper, x))
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

challenge_seed <- function(session_seed, game, round_id = 1L, salt = 0L) {
  text <- paste(as.integer(session_seed), game, as.integer(round_id), as.integer(salt), sep = "|")
  values <- utf8ToInt(enc2utf8(text))
  hash <- sum((values + 17L) * ((seq_along(values) * 7919L) %% 104729L))
  as.integer(hash %% (.Machine$integer.max - 1L)) + 1L
}

with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}

rotate_xy <- function(xy, angle) {
  rotation <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
  xy %*% rotation
}

challenge_data <- function(session_seed, game = c("mean", "pca", "regression", "kmeans"),
                           round_id = 1L, n = 72L, k = NULL) {
  game <- match.arg(game)
  with_seed(challenge_seed(session_seed, game, round_id, 101L), {
    round_type <- ((as.integer(round_id) - 1L) %% 4L) + 1L

    if (game == "mean") {
      if (round_type == 1L) {
        centers <- matrix(c(-1.5, -0.8, 1.25, -0.65, 0.1, 1.35), 3, 2, byrow = TRUE)
        group <- sample(1:3, n, replace = TRUE, prob = c(.44, .34, .22))
        xy <- centers[group, ] + cbind(stats::rnorm(n, sd = .62), stats::rnorm(n, sd = .48))
      } else if (round_type == 2L) {
        theta <- stats::runif(n, 0, 2 * pi)
        radius <- 1.65 + stats::rnorm(n, sd = .32)
        xy <- cbind(.55 + radius * cos(theta), -.25 + radius * sin(theta))
        group <- rep(1L, n)
      } else if (round_type == 3L) {
        theta <- stats::runif(n, -.2 * pi, 1.25 * pi)
        xy <- cbind(1.7 * cos(theta), sin(theta) + .28 * theta)
        group <- ifelse(theta < .55 * pi, 1L, 2L)
      } else {
        arm <- sample(1:2, n, replace = TRUE, prob = c(.62, .38))
        t <- stats::rnorm(n, sd = 1.35)
        xy <- cbind(ifelse(arm == 1, t, stats::rnorm(n, sd = .28)),
                    ifelse(arm == 1, stats::rnorm(n, sd = .28), t))
        xy[, 1] <- xy[, 1] + .35
        group <- arm
      }
      xy <- xy + matrix(stats::rnorm(2 * n, sd = .12 + .04 * round_type), ncol = 2)
    }

    if (game == "pca") {
      t <- stats::rnorm(n, sd = 1.45)
      if (round_type == 1L) {
        xy <- cbind(t, .18 * t + stats::rnorm(n, sd = .34))
      } else if (round_type == 2L) {
        xy <- cbind(t, .22 * t^2 * sign(t) + stats::rnorm(n, sd = .42))
      } else if (round_type == 3L) {
        branch <- sample(c(-1, 1), n, replace = TRUE)
        xy <- cbind(t, .55 * t + .62 * branch + stats::rnorm(n, sd = .28))
      } else {
        xy <- cbind(t, -.25 * t + stats::rnorm(n, sd = .52 + .12 * abs(t)))
        outliers <- sample(seq_len(n), 5)
        xy[outliers, 2] <- xy[outliers, 2] + stats::rnorm(5, 0, 1.15)
      }
      xy <- rotate_xy(xy, stats::runif(1, -.9, .9))
      xy <- xy + matrix(stats::rnorm(2 * n, sd = .08), ncol = 2)
      group <- rep(1L, n)
    }

    if (game == "regression") {
      x <- stats::runif(n, -2.25, 2.25)
      slope <- c(1, -1, 1, -1)[round_type] * stats::runif(1, .55, 1.35)
      intercept <- stats::runif(1, -.55, .55)
      sigma <- switch(round_type, .34, .48, .22 + .30 * abs(x), .38)
      curve <- if (round_type == 4L) .16 * (x^2 - mean(x^2)) else 0
      y <- intercept + slope * x + curve + stats::rnorm(n, sd = sigma)
      xy <- cbind(x, y)
      group <- rep(1L, n)
    }

    if (game == "kmeans") {
      k <- as.integer(k %||% 3L)
      angles <- seq(0, 2 * pi, length.out = k + 1L)[-(k + 1L)] + stats::runif(1, -.5, .5)
      radius <- 1.35 + .18 * k
      centers <- cbind(radius * cos(angles), .78 * radius * sin(angles))
      probabilities <- stats::runif(k, .65, 1.35); probabilities <- probabilities / sum(probabilities)
      group <- sample(seq_len(k), n, replace = TRUE, prob = probabilities)
      xy <- matrix(0, n, 2)
      for (cluster in seq_len(k)) {
        ids <- which(group == cluster)
        if (!length(ids)) next
        base <- cbind(stats::rnorm(length(ids), sd = .74), stats::rnorm(length(ids), sd = .42 + .06 * k))
        base <- rotate_xy(base, angles[cluster] / 2 + stats::runif(1, -.35, .35))
        xy[ids, ] <- sweep(base, 2, centers[cluster, ], "+")
      }
      # Puntos puente y ruido gaussiano hacen que la frontera sea deliberadamente ambigua.
      bridge_n <- max(4L, round(.09 * n))
      bridge_ids <- sample(seq_len(n), bridge_n)
      pair_a <- sample(seq_len(k), bridge_n, replace = TRUE)
      pair_b <- (pair_a %% k) + 1L
      alpha <- stats::runif(bridge_n, .25, .75)
      xy[bridge_ids, ] <- centers[pair_a, ] * alpha + centers[pair_b, ] * (1 - alpha) +
        matrix(stats::rnorm(2 * bridge_n, sd = .28), ncol = 2)
      xy <- xy + matrix(stats::rnorm(2 * n, sd = .10), ncol = 2)
    }

    data.frame(x = xy[, 1], y = xy[, 2], group = group)
  })
}

mean_metrics <- function(data, point) {
  xy <- as.matrix(data[, c("x", "y")])
  point <- as.numeric(point)[1:2]
  raw <- mean(rowSums(sweep(xy, 2, point, "-")^2))
  optimum_point <- colMeans(xy)
  optimum <- mean(rowSums(sweep(xy, 2, optimum_point, "-")^2))
  excess <- max(0, raw - optimum)
  points <- clamp(round(1000 * exp(-4 * excess / max(optimum, 1e-9))))
  list(raw = raw, optimum = optimum, points = points, solution = optimum_point)
}

kmeans_metrics <- function(data, centers) {
  xy <- as.matrix(data[, c("x", "y")])
  centers <- as.matrix(centers)
  distances <- vapply(seq_len(nrow(centers)), function(i) {
    rowSums(sweep(xy, 2, centers[i, ], "-")^2)
  }, numeric(nrow(xy)))
  if (is.null(dim(distances))) distances <- matrix(distances, ncol = 1)
  assignment <- max.col(-distances, ties.method = "first")
  raw <- mean(distances[cbind(seq_len(nrow(xy)), assignment)])
  optimum_fit <- stats::kmeans(xy, centers = nrow(centers), nstart = 60, iter.max = 150)
  optimum <- optimum_fit$tot.withinss / nrow(xy)
  points <- clamp(round(1000 * optimum / max(raw, optimum)))
  list(raw = raw, optimum = optimum, points = points, assignment = assignment,
       solution = optimum_fit$centers)
}

line_from_points <- function(points) {
  points <- as.matrix(points)
  stopifnot(nrow(points) == 2L, ncol(points) == 2L)
  direction <- points[2, ] - points[1, ]
  norm <- sqrt(sum(direction^2))
  if (!is.finite(norm) || norm < 1e-8) return(NULL)
  list(origin = points[1, ], direction = direction / norm)
}

pca_metrics <- function(data, points) {
  line <- line_from_points(points)
  if (is.null(line)) return(NULL)
  xy <- as.matrix(data[, c("x", "y")])
  centered <- sweep(xy, 2, line$origin, "-")
  along <- as.numeric(centered %*% line$direction)
  projected <- sweep(outer(along, line$direction), 2, line$origin, "+")
  raw <- mean(rowSums((xy - projected)^2))
  covariance <- stats::cov(xy)
  eigenvalues <- eigen(covariance, symmetric = TRUE, only.values = TRUE)$values
  optimum <- min(eigenvalues) * (nrow(xy) - 1) / nrow(xy)
  points_score <- clamp(round(1000 * optimum / max(raw, optimum)))
  list(raw = raw, optimum = optimum, points = points_score, projected = projected,
       line = line)
}

regression_metrics <- function(data, points) {
  line <- line_from_points(points)
  if (is.null(line) || abs(line$direction[1]) < 1e-8) return(NULL)
  xy <- as.matrix(data[, c("x", "y")])
  slope <- line$direction[2] / line$direction[1]
  intercept <- line$origin[2] - slope * line$origin[1]
  fitted <- intercept + slope * xy[, 1]
  raw <- mean((xy[, 2] - fitted)^2)
  fit <- stats::lm(y ~ x, data = data)
  optimum <- mean(stats::residuals(fit)^2)
  points_score <- clamp(round(1000 * optimum / max(raw, optimum)))
  list(raw = raw, optimum = optimum, points = points_score, fitted = fitted,
       slope = slope, intercept = intercept, solution = stats::coef(fit))
}

curse_challenge <- function(session_seed, round_id = 1L, dimensions = 10L,
                            n_candidates = 6L) {
  dimensions <- as.integer(dimensions)
  with_seed(challenge_seed(session_seed, "curse", round_id, 700L + dimensions), {
    target <- stats::runif(dimensions)
    candidates <- matrix(stats::runif(n_candidates * dimensions), nrow = n_candidates)
    distances <- sqrt(rowSums(sweep(candidates, 2, target, "-")^2))
    palette_start <- stats::runif(1, 0, 360)
    palette_span <- sample(c(-190, -145, 125, 170, 210), 1)
    column_shift <- stats::runif(dimensions, -24, 24)
    list(target = target, candidates = candidates, distances = distances,
         nearest = which.min(distances), contrast =
           (max(distances) - min(distances)) / max(min(distances), 1e-9),
         palette_start = palette_start, palette_span = palette_span,
         column_shift = column_shift)
  })
}

curse_points <- function(distances, choice, seconds = 0) {
  choice <- as.integer(choice)
  if (is.na(choice) || choice < 1L || choice > length(distances)) return(0L)
  accuracy <- min(distances) / distances[choice]
  as.integer(clamp(round(1000 * accuracy)))
}

time_multiplier <- function(seconds, decay = 45) {
  seconds <- max(0, as.numeric(seconds %||% 0))
  0.9 + 0.1 * exp(-seconds / decay)
}

apply_time_penalty <- function(points, seconds, decay = 45) {
  as.integer(clamp(round(as.numeric(points) * time_multiplier(seconds, decay))))
}
