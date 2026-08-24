test_that("el servidor avanza rondas y acumula el marcador de sesión", {
  old_dir <- setwd(testthat::test_path("..", ".."))
  on.exit(setwd(old_dir), add = TRUE)
  app_env <- new.env(parent = globalenv())
  sys.source("app.R", envir = app_env)

  shiny::testServer(app_env$server, {
    session$setInputs(player_name = "Ada", identity_save = 1)
    session$flushReact()
    expect_equal(output$player_context, "Ada")
    expect_match(output$curse_board$html, "2 coordenadas", fixed = TRUE)
    expect_equal(sort(k_order()), 2:5)

    session$setInputs(mean_click = list(x = 0, y = 0))
    session$flushReact()
    expect_match(output$mean_score$html, "puntos base / 1000", fixed = TRUE)
    session$setInputs(mean_submit = 1)
    session$flushReact()
    expect_equal(output$mean_round_label, "2/4")
    expect_match(output$session_scoreboard$html, "1/4 rondas", fixed = TRUE)

    session$setInputs(curse_pick = list(choice = 1, seconds = 4, nonce = 1))
    session$flushReact()
    expect_match(output$curse_feedback$html, "Contraste relativo", fixed = TRUE)
    session$setInputs(curse_submit = 1)
    session$flushReact()
    expect_match(output$curse_dimension_badge$html, "<strong>5</strong>", fixed = TRUE)

    for (round in 2:5) {
      expect_equal(c(2L, 5L, 10L, 20L, 50L)[progress$curse], c(5L, 10L, 20L, 50L)[round - 1L])
      session$setInputs(curse_pick = list(choice = 1, seconds = 4, nonce = round))
      session$flushReact()
      session$setInputs(curse_submit = round)
      session$flushReact()
    }
    expect_true(completed$curse)
    expect_equal(nrow(session_scores()[session_scores()$game == "curse", ]), 5)

    order <- k_order()
    for (round in 1:4) {
      expect_equal(current_k(), order[round])
      session$setInputs(kmeans_reveal = round)
      session$flushReact()
      expect_equal(state$kmeans_result$points, 0L)
      session$setInputs(kmeans_submit = round)
      session$flushReact()
    }
    expect_true(completed$kmeans)
    expect_equal(sort(order), 2:5)
  })
})

test_that("revelar una solución concede cero puntos", {
  old_dir <- setwd(testthat::test_path("..", ".."))
  on.exit(setwd(old_dir), add = TRUE)
  app_env <- new.env(parent = globalenv())
  sys.source("app.R", envir = app_env)

  shiny::testServer(app_env$server, {
    session$setInputs(player_name = "Noether", identity_save = 1)
    session$flushReact()
    session$setInputs(mean_reveal = 1)
    session$flushReact()
    expect_equal(state$mean_result$points, 0L)
    expect_true(forfeited$mean)
    locked_point <- state$mean_point

    session$setInputs(mean_click = list(x = 99, y = 99), mean_reset = 1)
    session$flushReact()
    expect_equal(state$mean_result$points, 0L)
    expect_equal(state$mean_point, locked_point)

    session$setInputs(mean_submit = 1)
    session$flushReact()
    expect_equal(session_scores()$points[1], 0L)
    expect_false(forfeited$mean)
  })
})

test_that("el servidor aplica el mismo descuento temporal al cerrar", {
  old_dir <- setwd(testthat::test_path("..", ".."))
  on.exit(setwd(old_dir), add = TRUE)
  app_env <- new.env(parent = globalenv())
  sys.source("app.R", envir = app_env)

  shiny::testServer(app_env$server, {
    session$setInputs(player_name = "Fisher", identity_save = 1)
    session$flushReact()
    round_started$mean <- as.numeric(Sys.time()) - 1e6
    session$setInputs(mean_click = list(x = 0, y = 0))
    session$flushReact()
    base <- state$mean_result$points
    session$setInputs(mean_submit = 1)
    session$flushReact()

    expect_equal(session_scores()$points[1], as.integer(round(base * 0.9)))
    expect_equal(session_scores()$time_lost[1], base - session_scores()$points[1])
    expect_gte(session_scores()$seconds[1], 1e6)
  })
})

test_that("la interfaz contiene un cronómetro para cada juego y ninguna insignia nueva", {
  old_dir <- setwd(testthat::test_path("..", ".."))
  on.exit(setwd(old_dir), add = TRUE)
  app_env <- new.env(parent = globalenv())
  sys.source("app.R", envir = app_env)
  html <- paste(vapply(
    c("mean", "kmeans", "pca", "regression", "shadows", "curse"),
    function(game) as.character(get(paste0(game, "_view"), envir = app_env)),
    character(1)
  ), collapse = "")

  for (game in c("mean", "kmeans", "pca", "regression", "shadows", "curse")) {
    expect_match(html, sprintf('id="%s_timer"', game), fixed = TRUE)
  }
  expect_false(grepl("new-badge", html, fixed = TRUE))
  expect_equal(app_env$round_totals[["shadows"]], 4L)
  expect_identical(app_env$shadow_targets, c("circle", "square", "triangle", "hexagon"))

  finalized <- as.character(app_env$score_ui(800, 2, 1, "error", "mean", 900, TRUE))
  expect_match(finalized, "time-loss", fixed = TRUE)
  expect_match(finalized, "data-final-points=\"800\"", fixed = TRUE)
  expect_match(finalized, "−100 puntos por tiempo", fixed = TRUE)
})
