source(testthat::test_path("..", "..", "R", "game_logic.R"))

test_that("una semilla reproduce la ronda y otra semilla la cambia", {
  a1 <- challenge_data(12345, "mean", 3)
  a2 <- challenge_data(12345, "mean", 3)
  b <- challenge_data(54321, "mean", 3)
  expect_identical(a1, a2)
  expect_false(isTRUE(all.equal(a1, b)))
})

test_that("las cuatro rondas de cada juego 2D tienen datos diferentes", {
  for (game in c("mean", "pca", "regression")) {
    rounds <- lapply(1:4, function(round) challenge_data(8128, game, round))
    expect_length(unique(vapply(rounds, function(x) paste(round(x$x, 3), collapse = ","), character(1))), 4)
  }
})

test_that("las soluciones estadísticas reciben aproximadamente 1000 puntos", {
  mean_data <- challenge_data(99, "mean", 1)
  expect_equal(mean_metrics(mean_data, colMeans(mean_data[, c("x", "y")]))$points, 1000)

  k_data <- challenge_data(99, "kmeans", 2, k = 4)
  fit_k <- kmeans(k_data[, c("x", "y")], centers = 4, nstart = 100)
  expect_gte(kmeans_metrics(k_data, fit_k$centers)$points, 995)

  pca_data <- challenge_data(99, "pca", 3)
  xy <- as.matrix(pca_data[, c("x", "y")]); center <- colMeans(xy)
  direction <- eigen(cov(xy), symmetric = TRUE)$vectors[, 1]
  expect_equal(pca_metrics(pca_data, rbind(center - direction, center + direction))$points, 1000)

  reg_data <- challenge_data(99, "regression", 4)
  fit_lm <- lm(y ~ x, data = reg_data); x <- range(reg_data$x)
  line <- cbind(x, predict(fit_lm, newdata = data.frame(x = x)))
  expect_equal(regression_metrics(reg_data, line)$points, 1000)
})

test_that("k-means admite todos los k del recorrido", {
  for (k in 2:5) {
    data <- challenge_data(404, "kmeans", k - 1, k = k)
    fit <- kmeans(data[, c("x", "y")], centers = k, nstart = 30)
    result <- kmeans_metrics(data, fit$centers)
    expect_equal(nrow(result$solution), k)
    expect_equal(length(result$assignment), nrow(data))
  }
})

test_that("la concentración de distancias aumenta con la dimensión", {
  average_contrast <- function(p) mean(vapply(1:100, function(round) curse_challenge(2026, round, p)$contrast, numeric(1)))
  expect_gt(average_contrast(2), average_contrast(50))
})

test_that("el reto dimensional cambia de paleta y puntúa el vecino exacto", {
  a <- curse_challenge(77, 1, 20); b <- curse_challenge(77, 2, 20)
  expect_false(isTRUE(all.equal(a$palette_start, b$palette_start)))
  expect_length(a$column_shift, 20)
  exact <- curse_points(a$distances, a$nearest, seconds = 0)
  wrong <- curse_points(a$distances, which.max(a$distances), seconds = 0)
  expect_equal(exact, 1000)
  expect_gt(exact, wrong)
})

test_that("la penalización temporal común nunca supera el diez por ciento", {
  expect_equal(time_multiplier(0), 1)
  expect_lt(time_multiplier(45), 1)
  expect_gt(time_multiplier(45), 0.9)
  expect_equal(apply_time_penalty(1000, 0), 1000L)
  expect_equal(apply_time_penalty(1000, 1e6), 900L)
  expect_gte(apply_time_penalty(537, 1e6), as.integer(round(537 * 0.9)))
  expect_gt(apply_time_penalty(1000, 10), apply_time_penalty(1000, 100))
})

test_that("alta dimensión deja el tiempo para la regla común", {
  ch <- curse_challenge(91, 2, 20)
  expect_equal(curse_points(ch$distances, ch$nearest, seconds = 0), 1000L)
  expect_equal(curse_points(ch$distances, ch$nearest, seconds = 999), 1000L)
})
