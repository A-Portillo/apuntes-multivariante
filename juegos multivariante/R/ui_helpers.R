game_names <- c(
  mean = "Centro perfecto",
  kmeans = "Constelaciones",
  pca = "Dimensión perdida",
  regression = "Premonición",
  shadows = "Cuatro sombras",
  curse = "Vecinos en fuga"
)

game_topics <- c(
  mean = "Media · varianza",
  kmeans = "Clustering · k-means",
  pca = "Reducción · PCA",
  regression = "Predicción · regresión",
  shadows = "Proyecciones · geometría 3D",
  curse = "Alta dimensión · vecinos"
)

game_icons <- c(mean = "◎", kmeans = "✣", pca = "↘", regression = "╱", shadows = "◩", curse = "⌗")

game_card <- function(id, description) {
  tags$button(
    class = "game-card", type = "button", `data-game` = id,
    tags$div(class = "card-top",
      tags$span(class = "game-icon", game_icons[[id]])
    ),
    tags$div(class = "eyebrow", game_topics[[id]]),
    tags$h3(game_names[[id]]),
    tags$p(description),
    tags$span(class = "play-link", "Jugar", tags$span("→"))
  )
}

mission <- function(kicker, title, text, ...) {
  tags$section(
    class = "mission-card",
    tags$div(class = "eyebrow", kicker),
    tags$h2(title),
    tags$p(text),
    tags$div(class = "mission-actions", ...)
  )
}

score_box <- function(output_id) {
  tags$div(class = "score-box", uiOutput(output_id))
}

disabled_action_button <- function(id, label) {
  htmltools::tagAppendAttributes(
    actionButton(id, label, class = "btn-primary-app"),
    disabled = "disabled"
  )
}

score_ui <- function(points = NULL, raw = NULL, optimum = NULL, label = "error",
                     game = NULL, base_points = points, finalized = FALSE,
                     forfeited = FALSE) {
  if (is.null(points)) {
    return(tagList(
      tags$span(class = "score-empty", "Haz tu primer intento"),
      tags$small("Hasta 1000 puntos · tiempo: máximo −10%")
    ))
  }
  gap <- if (!is.null(optimum) && is.finite(optimum)) raw / max(optimum, 1e-9) else NA_real_
  loss <- if (isTRUE(forfeited)) {
    tags$div(class = "time-loss forfeited", "Solución mostrada · ronda bloqueada en 0")
  } else if (!is.null(game)) {
    base_points <- as.integer(base_points %||% points)
    final_points <- if (isTRUE(finalized)) as.integer(points) else NULL
    loss_tag <- tags$div(
      class = "time-loss", id = paste0(game, "_time_loss"),
      `data-base-points` = base_points,
      sprintf("−%d puntos por tiempo%s", if (is.null(final_points)) 0L else base_points - final_points,
              if (is.null(final_points)) " ahora" else "")
    )
    if (!is.null(final_points)) {
      loss_tag <- htmltools::tagAppendAttributes(loss_tag, `data-final-points` = final_points)
    }
    loss_tag
  }
  tagList(
    tags$div(class = "score-number", formatC(as.integer(points), format = "d", big.mark = ".", decimal.mark = ",")),
    tags$div(class = "score-caption", if (isTRUE(finalized)) "puntos finales / 1000" else "puntos base / 1000"),
    tags$div(class = "score-detail",
      sprintf("%s: %.3f", label, raw),
      if (is.finite(gap)) sprintf(" · %.2fx el óptimo", gap)
    ),
    loss
  )
}

game_shell <- function(id, mission_ui, play_ui, score_output, help_text) {
  tags$section(
    id = paste0("panel-", id), class = "view game-view", `data-view` = id,
    tags$button(class = "back-button", type = "button", `data-game` = "home", "← Todos los juegos"),
    tags$div(class = "game-layout",
      tags$aside(class = "game-sidebar",
        mission_ui,
        score_box(score_output),
        tags$div(class = "micro-help", tags$strong("Idea clave"), tags$p(help_text))
      ),
      tags$main(class = "play-surface", play_ui)
    )
  )
}

plot_controls <- function(prefix, primary = NULL) {
  tags$div(class = "plot-toolbar",
    tags$span(class = "round-chip", "Ronda ", textOutput(paste0(prefix, "_round_label"), inline = TRUE)),
    tags$span(class = "timer-chip", "⏱ ", tags$span(id = paste0(prefix, "_timer"), "0.0"), " s"),
    tags$span(class = "toolbar-spacer", `aria-hidden` = "true"),
    if (!is.null(primary)) primary,
    actionButton(paste0(prefix, "_reset"), "Reiniciar", class = "btn-ghost"),
    actionButton(paste0(prefix, "_reveal"), "Rendirse y ver solución", class = "btn-ghost")
  )
}
