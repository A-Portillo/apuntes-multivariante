library(shiny)
library(bslib)
library(htmltools)

source("R/game_logic.R", local = TRUE)
source("R/plotting.R", local = TRUE)
source("R/ui_helpers.R", local = TRUE)

app_theme <- bs_theme(
  version = 5, bg = "#F4F1EA", fg = "#14213D", primary = "#705CF6",
  secondary = "#08A88A", base_font = font_collection("system-ui"),
  heading_font = font_collection("Georgia", "serif"), border_radius = "1rem"
)

home_view <- tags$section(
  id = "panel-home", class = "view home-view active", `data-view` = "home",
  tags$div(class = "hero",
    tags$div(class = "hero-copy",
      tags$div(class = "eyebrow hero-kicker", "ARCADE DE ESTADÍSTICA MULTIVARIANTE"),
      tags$h1("Juega con los datos.", tags$br(), tags$em("Piensa en grande.")),
      tags$p("Seis series de retos. Cada sesión crea datos, formas, ruido y órdenes nuevos para que cuente la intuición, no la memoria."),
      tags$div(class = "hero-actions",
        tags$button(class = "btn-primary-app", type = "button", `data-game` = "mean", "Empezar a jugar", tags$span("→")),
        tags$button(class = "btn-quiet", type = "button", `data-open-league` = "true", "Ver marcador")
      )
    ),
    tags$div(class = "hero-art", `aria-hidden` = "true",
      tags$div(class = "orbit orbit-a"), tags$div(class = "orbit orbit-b"),
      tags$div(class = "data-dot dot-a"), tags$div(class = "data-dot dot-b"),
      tags$div(class = "data-dot dot-c"), tags$div(class = "hero-score", "25K", tags$small("MÁXIMO"))
    )
  ),
  tags$div(class = "section-heading",
    tags$div(tags$div(class = "eyebrow", "ELIGE TU RETO"), tags$h2("La colección")),
    tags$p("Cierra cada ronda para sumarla al marcador de esta sesión. Al recargar la página comienza una partida nueva.")
  ),
  tags$div(class = "game-grid",
    game_card("mean", "Cuatro distribuciones distintas para encontrar su centro."),
    game_card("kmeans", "Cuatro rondas ambiguas con valores de k diferentes."),
    game_card("pca", "Cuatro nubes y formas para comprimir en una dirección."),
    game_card("regression", "Cuatro relaciones nuevas con pendientes y ruido variables."),
    game_card("shadows", "Círculo, cuadrado, triángulo y la proyección hexagonal de un cubo."),
    game_card("curse", "Recorre cinco dimensiones con paletas cambiantes.")
  )
)

mean_view <- game_shell(
  "mean",
  mission("MISIÓN 01 · MEDIA", "Centro perfecto",
    "Supera cuatro distribuciones. Haz clic donde colocarías un único representante; las líneas largas penalizan mucho.",
    disabled_action_button("mean_submit", "Cerrar ronda")),
  tagList(plot_controls("mean"), plotOutput("mean_plot", click = "mean_click", height = "620px")),
  "mean_score",
  "La media de Fréchet minimiza la suma de distancias euclídeas al cuadrado, incluso cuando la nube tiene una forma poco habitual."
)

kmeans_view <- game_shell(
  "kmeans",
  mission("MISIÓN 02 · CLUSTERING", "Constelaciones",
    "Completa cuatro rondas. El valor de k cambia y habrá solapamientos, grupos alargados y puntos puente.",
    disabled_action_button("kmeans_submit", "Cerrar ronda")),
  tagList(
    plot_controls("kmeans", uiOutput("kmeans_k_badge")),
    plotOutput("kmeans_plot", click = "kmeans_click", height = "620px")
  ),
  "kmeans_score",
  "Cuando los clusters se solapan, k-means sigue buscando centroides que minimicen la inercia, aunque la partición deje de ser evidente."
)

pca_view <- game_shell(
  "pca",
  mission("MISIÓN 03 · PCA", "Dimensión perdida",
    "Traza una recta con dos clics en cada una de las cuatro nubes. Minimiza las proyecciones ortogonales.",
    disabled_action_button("pca_submit", "Cerrar ronda")),
  tagList(plot_controls("pca"), plotOutput("pca_plot", click = "pca_click", height = "620px")),
  "pca_score",
  "La primera componente principal conserva tanta variación como puede, aunque la nube sea curva, tenga ramas o contenga atípicos."
)

regression_view <- game_shell(
  "regression",
  mission("MISIÓN 04 · REGRESIÓN", "Premonición",
    "Resuelve cuatro relaciones con pendientes, dispersión y curvatura distintas. Aquí solo cuentan los errores verticales.",
    disabled_action_button("regression_submit", "Cerrar ronda")),
  tagList(plot_controls("regression"), plotOutput("regression_plot", click = "regression_click", height = "620px")),
  "regression_score",
  "Mínimos cuadrados minimiza los residuos verticales. El ruido y la heterocedasticidad dificultan estimar la recta a simple vista."
)

shadows_view <- game_shell(
  "shadows",
  mission("MISIÓN 05 · PROYECCIONES", "Cuatro sombras",
    "Consigue tres proyecciones del sólido compuesto y termina con la proyección hexagonal de un cubo.",
    disabled_action_button("shadow_save", "Cerrar ronda")),
  tagList(
    tags$div(class = "canvas-toolbar",
      tags$div(class = "target-card", tags$span("OBJETIVO"), tags$strong(id = "shadow_target", "CÍRCULO")),
      tags$span(class = "round-chip", "Ronda ", textOutput("shadows_round_label", inline = TRUE)),
      tags$span(class = "timer-chip", "⏱ ", tags$span(id = "shadows_timer", "0.0"), " s"),
      tags$button(id = "shadow_submit", type = "button", class = "btn-primary-app", "Evaluar"),
      tags$button(id = "shadow_restart", type = "button", class = "btn-ghost", "Reiniciar orientación"),
      tags$span(class = "drag-hint", "Arrastra para girar · doble clic reinicia")
    ),
    tags$canvas(id = "shadow_canvas", class = "shadow-canvas", tabindex = "0"),
    tags$div(class = "shape-legend",
      tags$span(tags$i(class = "legend-circle"), "círculo"),
      tags$span(tags$i(class = "legend-square"), "cuadrado"),
      tags$span(tags$i(class = "legend-triangle"), "triángulo")
    )
  ),
  "shadow_score",
  "Una proyección conserva unas propiedades y oculta otras. La dirección desde la que observamos es parte del análisis."
)

curse_view <- game_shell(
  "curse",
  mission("MISIÓN 06 · ALTA DIMENSIÓN", "Vecinos en fuga",
    "Recorre 2, 5, 10, 20 y 50 dimensiones. La paleta cambia en cada ronda, pero es coherente dentro de cada columna.",
    disabled_action_button("curse_submit", "Cerrar ronda")),
  tagList(
    tags$div(class = "curse-toolbar",
      uiOutput("curse_dimension_badge"),
      actionButton("curse_reset", "Reiniciar ronda", class = "btn-ghost"),
      tags$div(class = "timer-chip", "⏱ ", tags$span(id = "curse_timer", "0.0"), " s")
    ),
    uiOutput("curse_board"), uiOutput("curse_feedback")
  ),
  "curse_score",
  "Al aumentar la dimensión, las distancias se concentran y «cerca» y «lejos» discriminan cada vez menos."
)

score_drawer <- tags$aside(
  id = "league_drawer", class = "league-drawer", `aria-hidden` = "true",
  tags$div(class = "drawer-head",
    tags$div(tags$div(class = "eyebrow", "PARTIDA ACTUAL"), tags$h2("Marcador de sesión")),
    tags$button(type = "button", class = "drawer-close", `data-close-league` = "true", "×")
  ),
  uiOutput("session_scoreboard"),
  tags$p(class = "session-note", "Este marcador vive solo en esta pestaña. No se envía ni se guarda en ninguna base de datos.")
)

identity_modal <- div(
  id = "identity_modal", class = "identity-overlay",
  div(class = "identity-card",
    div(class = "identity-mark", "Σ"),
    div(class = "eyebrow", "NUEVA SESIÓN"),
    h2("Ponle nombre a tu partida"),
    p("El alias identifica el marcador mientras esta pestaña permanezca abierta. No uses datos personales."),
    textInput("player_name", "Alias", placeholder = "ej. GaussVeloz"),
    actionButton("identity_save", "Empezar una sesión nueva", class = "btn-primary-app")
  )
)

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app.js", defer = NA)
  ),
  tags$header(class = "topbar",
    tags$button(class = "brand", type = "button", `data-game` = "home",
      tags$span(class = "brand-mark", "Σ"), tags$span("DATA", tags$strong("ARCADE"))
    ),
    tags$button(class = "topbar-context", type = "button", `data-edit-identity` = "true", title = "Iniciar otra sesión",
      tags$span(class = "live-dot"), textOutput("player_context", inline = TRUE)
    ),
    tags$button(class = "league-button", type = "button", `data-open-league` = "true", "🎯 Marcador")
  ),
  tags$main(class = "app-main", home_view, mean_view, kmeans_view, pca_view, regression_view, shadows_view, curse_view),
  tags$div(id = "drawer_scrim", class = "drawer-scrim", `data-close-league` = "true"),
  score_drawer, identity_modal
)

round_totals <- c(mean = 4L, kmeans = 4L, pca = 4L, regression = 4L, shadows = 4L, curse = 5L)
submit_ids <- c(mean = "mean_submit", kmeans = "kmeans_submit", pca = "pca_submit",
                regression = "regression_submit", shadows = "shadow_save", curse = "curse_submit")
curse_dimensions <- c(2L, 5L, 10L, 20L, 50L)
shadow_targets <- c("circle", "square", "triangle", "hexagon")

empty_session_scores <- function() {
  data.frame(game = character(), round = integer(), points = integer(), raw = numeric(),
             seconds = numeric(), base_points = integer(), time_lost = integer(),
             stringsAsFactors = FALSE)
}

clean_alias <- function(x) {
  x <- gsub("[^[:alnum:] _.-]", "", trimws(as.character(x %||% "")))
  x <- substr(x, 1L, 28L)
  if (nzchar(x)) x else "Anónimo"
}

server <- function(input, output, session) {
  identity <- reactiveValues(player = "")
  progress <- reactiveValues(mean = 1L, kmeans = 1L, pca = 1L, regression = 1L, shadows = 1L, curse = 1L)
  completed <- reactiveValues(mean = FALSE, kmeans = FALSE, pca = FALSE, regression = FALSE, shadows = FALSE, curse = FALSE)
  forfeited <- reactiveValues(mean = FALSE, kmeans = FALSE, pca = FALSE, regression = FALSE, shadows = FALSE, curse = FALSE)
  round_started <- reactiveValues(mean = NA_real_, kmeans = NA_real_, pca = NA_real_, regression = NA_real_, shadows = NA_real_, curse = NA_real_)
  state <- reactiveValues(
    mean_point = NULL, mean_result = NULL, mean_reveal = FALSE,
    kmeans_centers = NULL, kmeans_result = NULL, kmeans_reveal = FALSE,
    pca_clicks = NULL, pca_result = NULL, pca_reveal = FALSE,
    regression_clicks = NULL, regression_result = NULL, regression_reveal = FALSE,
    shadow_result = NULL, curse_choice = NULL, curse_result = NULL
  )
  session_seed <- reactiveVal(sample.int(.Machine$integer.max - 1L, 1L))
  k_order <- reactiveVal(sample(2:5))
  session_scores <- reactiveVal(empty_session_scores())

  ensure_round_timer <- function(game) {
    if (!is.finite(round_started[[game]])) {
      round_started[[game]] <- as.numeric(Sys.time())
      session$sendCustomMessage("resetGameTimer", list(game = game))
    }
    invisible(round_started[[game]])
  }

  start_round_timer <- function(game) {
    round_started[[game]] <- as.numeric(Sys.time())
    session$sendCustomMessage("resetGameTimer", list(game = game))
  }

  elapsed_round <- function(game) {
    ensure_round_timer(game)
    max(0, as.numeric(Sys.time()) - round_started[[game]])
  }

  reset_game <- function(game, new_round = FALSE) {
    if (new_round) forfeited[[game]] <- FALSE
    if (game == "mean") { state$mean_point <- state$mean_result <- NULL; state$mean_reveal <- FALSE }
    if (game == "kmeans") { state$kmeans_centers <- state$kmeans_result <- NULL; state$kmeans_reveal <- FALSE }
    if (game == "pca") { state$pca_clicks <- state$pca_result <- NULL; state$pca_reveal <- FALSE }
    if (game == "regression") { state$regression_clicks <- state$regression_result <- NULL; state$regression_reveal <- FALSE }
    if (game == "shadows") state$shadow_result <- NULL
    if (game == "curse") state$curse_choice <- state$curse_result <- NULL
    shinyjs_toggle_button(session, submit_ids[[game]], FALSE)
    shinyjs_toggle_button(session, paste0(game, "_reset"), !forfeited[[game]])
    if (game %in% c("mean", "kmeans", "pca", "regression")) {
      shinyjs_toggle_button(session, paste0(game, "_reveal"), !forfeited[[game]])
    }
    updateActionButton(session, submit_ids[[game]], label = sprintf("Cerrar ronda %d/%d", progress[[game]], round_totals[[game]]))
  }

  start_session <- function(player) {
    identity$player <- clean_alias(player)
    session_seed(sample.int(.Machine$integer.max - 1L, 1L))
    k_order(sample(2:5))
    session_scores(empty_session_scores())
    for (game in names(round_totals)) {
      progress[[game]] <- 1L; completed[[game]] <- FALSE; forfeited[[game]] <- FALSE
      round_started[[game]] <- NA_real_; reset_game(game, new_round = TRUE)
    }
    session$sendCustomMessage("clearGameTimers", list())
    session$sendCustomMessage("setShadowTarget", list(target = "circle"))
  }

  observeEvent(input$identity_save, {
    req(nzchar(trimws(input$player_name)))
    start_session(input$player_name)
    session$sendCustomMessage("saveIdentity", list(player = identity$player))
  })

  observeEvent(input$game_opened, {
    game <- as.character(input$game_opened$game %||% "")
    req(nzchar(identity$player), game %in% names(round_totals), !completed[[game]])
    ensure_round_timer(game)
  })

  output$player_context <- renderText(if (!nzchar(identity$player)) "Sin sesión" else identity$player)
  lapply(names(round_totals), function(game) local({
    id <- paste0(game, "_round_label")
    output[[id]] <- renderText(sprintf("%d/%d", progress[[game]], round_totals[[game]]))
  }))

  mean_data <- reactive(challenge_data(session_seed(), "mean", progress$mean))
  pca_data <- reactive(challenge_data(session_seed(), "pca", progress$pca))
  regression_data <- reactive(challenge_data(session_seed(), "regression", progress$regression))
  current_k <- reactive(k_order()[progress$kmeans])
  kmeans_data <- reactive(challenge_data(session_seed(), "kmeans", progress$kmeans, k = current_k()))

  output$kmeans_k_badge <- renderUI(tags$span(class = "k-badge", sprintf("k = %d", current_k())))
  output$curse_dimension_badge <- renderUI(tags$div(class = "dimension-stage",
    tags$span("DIMENSIÓN ACTUAL"), tags$strong(curse_dimensions[progress$curse]),
    tags$small(sprintf("ronda %d/%d", progress$curse, round_totals[["curse"]]))
  ))

  observeEvent(input$mean_click, {
    req(!completed$mean, !forfeited$mean); ensure_round_timer("mean")
    state$mean_point <- c(input$mean_click$x, input$mean_click$y)
    state$mean_result <- mean_metrics(mean_data(), state$mean_point); state$mean_reveal <- FALSE
    shinyjs_toggle_button(session, "mean_submit", TRUE)
  })
  observeEvent(input$mean_reset, { req(!completed$mean, !forfeited$mean); ensure_round_timer("mean"); reset_game("mean") })
  observeEvent(input$mean_reveal, {
    req(!completed$mean, !forfeited$mean); ensure_round_timer("mean"); forfeited$mean <- TRUE
    solution <- colMeans(mean_data()[, c("x", "y")]); state$mean_point <- solution
    state$mean_result <- mean_metrics(mean_data(), solution); state$mean_result$points <- 0L
    state$mean_reveal <- TRUE; shinyjs_toggle_button(session, "mean_submit", TRUE)
    shinyjs_toggle_button(session, "mean_reset", FALSE); shinyjs_toggle_button(session, "mean_reveal", FALSE)
  })
  output$mean_plot <- renderPlot(plot_mean_game(mean_data(), state$mean_point, state$mean_reveal), res = 110)
  output$mean_score <- renderUI(if (is.null(state$mean_result)) score_ui() else score_ui(state$mean_result$points, state$mean_result$raw, state$mean_result$optimum, "varianza", "mean", state$mean_result$base_points %||% state$mean_result$points, !is.null(state$mean_result$base_points), forfeited$mean))

  observeEvent(input$kmeans_click, {
    req(!completed$kmeans, !forfeited$kmeans); ensure_round_timer("kmeans")
    point <- c(input$kmeans_click$x, input$kmeans_click$y); centers <- state$kmeans_centers; k <- current_k()
    if (is.null(centers) || nrow(as.matrix(centers)) >= k) centers <- matrix(point, nrow = 1) else centers <- rbind(centers, point)
    state$kmeans_centers <- centers
    state$kmeans_result <- if (nrow(centers) == k) kmeans_metrics(kmeans_data(), centers) else NULL
    state$kmeans_reveal <- FALSE; shinyjs_toggle_button(session, "kmeans_submit", !is.null(state$kmeans_result))
  })
  observeEvent(input$kmeans_reset, { req(!completed$kmeans, !forfeited$kmeans); ensure_round_timer("kmeans"); reset_game("kmeans") })
  observeEvent(input$kmeans_reveal, {
    req(!completed$kmeans, !forfeited$kmeans); ensure_round_timer("kmeans"); forfeited$kmeans <- TRUE
    fit <- stats::kmeans(kmeans_data()[, c("x", "y")], current_k(), nstart = 60)
    state$kmeans_centers <- fit$centers; state$kmeans_result <- kmeans_metrics(kmeans_data(), fit$centers)
    state$kmeans_result$points <- 0L; state$kmeans_reveal <- TRUE; shinyjs_toggle_button(session, "kmeans_submit", TRUE)
    shinyjs_toggle_button(session, "kmeans_reset", FALSE); shinyjs_toggle_button(session, "kmeans_reveal", FALSE)
  })
  output$kmeans_plot <- renderPlot(plot_kmeans_game(kmeans_data(), state$kmeans_centers, state$kmeans_result, state$kmeans_reveal, current_k()), res = 110)
  output$kmeans_score <- renderUI(if (is.null(state$kmeans_result)) score_ui() else score_ui(state$kmeans_result$points, state$kmeans_result$raw, state$kmeans_result$optimum, "inercia", "kmeans", state$kmeans_result$base_points %||% state$kmeans_result$points, !is.null(state$kmeans_result$base_points), forfeited$kmeans))

  observeEvent(input$pca_click, {
    req(!completed$pca, !forfeited$pca); ensure_round_timer("pca")
    point <- c(input$pca_click$x, input$pca_click$y); clicks <- state$pca_clicks
    if (is.null(clicks) || nrow(as.matrix(clicks)) >= 2L) clicks <- matrix(point, nrow = 1) else clicks <- rbind(clicks, point)
    state$pca_clicks <- clicks; state$pca_result <- if (nrow(clicks) == 2L) pca_metrics(pca_data(), clicks) else NULL
    state$pca_reveal <- FALSE; shinyjs_toggle_button(session, "pca_submit", !is.null(state$pca_result))
  })
  observeEvent(input$pca_reset, { req(!completed$pca, !forfeited$pca); ensure_round_timer("pca"); reset_game("pca") })
  observeEvent(input$pca_reveal, {
    req(!completed$pca, !forfeited$pca); ensure_round_timer("pca"); forfeited$pca <- TRUE
    xy <- as.matrix(pca_data()[, c("x", "y")]); center <- colMeans(xy); direction <- eigen(cov(xy))$vectors[, 1]
    state$pca_clicks <- rbind(center - direction, center + direction); state$pca_result <- pca_metrics(pca_data(), state$pca_clicks)
    state$pca_result$points <- 0L; state$pca_reveal <- TRUE; shinyjs_toggle_button(session, "pca_submit", TRUE)
    shinyjs_toggle_button(session, "pca_reset", FALSE); shinyjs_toggle_button(session, "pca_reveal", FALSE)
  })
  output$pca_plot <- renderPlot(plot_pca_game(pca_data(), state$pca_clicks, state$pca_result, state$pca_reveal), res = 110)
  output$pca_score <- renderUI(if (is.null(state$pca_result)) score_ui() else score_ui(state$pca_result$points, state$pca_result$raw, state$pca_result$optimum, "error ortogonal", "pca", state$pca_result$base_points %||% state$pca_result$points, !is.null(state$pca_result$base_points), forfeited$pca))

  observeEvent(input$regression_click, {
    req(!completed$regression, !forfeited$regression); ensure_round_timer("regression")
    point <- c(input$regression_click$x, input$regression_click$y); clicks <- state$regression_clicks
    if (is.null(clicks) || nrow(as.matrix(clicks)) >= 2L) clicks <- matrix(point, nrow = 1) else clicks <- rbind(clicks, point)
    state$regression_clicks <- clicks; state$regression_result <- if (nrow(clicks) == 2L) regression_metrics(regression_data(), clicks) else NULL
    state$regression_reveal <- FALSE; shinyjs_toggle_button(session, "regression_submit", !is.null(state$regression_result))
  })
  observeEvent(input$regression_reset, { req(!completed$regression, !forfeited$regression); ensure_round_timer("regression"); reset_game("regression") })
  observeEvent(input$regression_reveal, {
    req(!completed$regression, !forfeited$regression); ensure_round_timer("regression"); forfeited$regression <- TRUE
    fit <- lm(y ~ x, regression_data()); x <- range(regression_data()$x)
    state$regression_clicks <- cbind(x, predict(fit, newdata = data.frame(x = x)))
    state$regression_result <- regression_metrics(regression_data(), state$regression_clicks); state$regression_result$points <- 0L
    state$regression_reveal <- TRUE; shinyjs_toggle_button(session, "regression_submit", TRUE)
    shinyjs_toggle_button(session, "regression_reset", FALSE); shinyjs_toggle_button(session, "regression_reveal", FALSE)
  })
  output$regression_plot <- renderPlot(plot_regression_game(regression_data(), state$regression_clicks, state$regression_result, state$regression_reveal), res = 110)
  output$regression_score <- renderUI(if (is.null(state$regression_result)) score_ui() else score_ui(state$regression_result$points, state$regression_result$raw, state$regression_result$optimum, "error vertical", "regression", state$regression_result$base_points %||% state$regression_result$points, !is.null(state$regression_result$base_points), forfeited$regression))

  observeEvent(input$shadow_attempt, {
    req(!completed$shadows); ensure_round_timer("shadows")
    attempt <- input$shadow_attempt; target <- shadow_targets[progress$shadows]
    req(identical(as.character(attempt$target), target))
    angle <- clamp(as.numeric(attempt$angle), 0, 90)
    points <- as.integer(clamp(round(1000 * exp(-angle / 12))))
    state$shadow_result <- list(points = points, raw = angle, optimum = 0)
    shinyjs_toggle_button(session, "shadow_save", TRUE)
  })
  observeEvent(input$shadow_reset, { req(!completed$shadows); ensure_round_timer("shadows"); state$shadow_result <- NULL; shinyjs_toggle_button(session, "shadow_save", FALSE) }, ignoreInit = TRUE)
  output$shadow_score <- renderUI(if (is.null(state$shadow_result)) score_ui() else score_ui(state$shadow_result$points, state$shadow_result$raw, NULL, "desviación angular (°)", "shadows", state$shadow_result$base_points %||% state$shadow_result$points, !is.null(state$shadow_result$base_points)))

  curse <- reactive(curse_challenge(session_seed(), progress$curse, curse_dimensions[progress$curse]))
  observeEvent(input$curse_reset, { req(!completed$curse); ensure_round_timer("curse"); reset_game("curse") })
  output$curse_board <- renderUI({
    ch <- curse(); dims <- ncol(ch$candidates)
    heat_row <- function(values) tags$div(class = "heat-strip", lapply(seq_along(values), function(j) {
      hue <- (ch$palette_start + values[j] * ch$palette_span + ch$column_shift[j]) %% 360
      light <- 38 + 24 * values[j]
      tags$i(style = sprintf("--h: %.1f; --l: %.1f%%", hue, light), title = sprintf("x%d = %.2f", j, values[j]))
    }))
    tags$div(class = "curse-board",
      tags$div(class = "dimension-caption", sprintf("%d coordenadas · paleta de esta ronda", dims)),
      tags$div(class = "target-row", tags$span(class = "row-label", "OBJETIVO"), heat_row(ch$target)),
      tags$div(class = "candidate-list", lapply(seq_len(nrow(ch$candidates)), function(i) {
        tags$button(type = "button", class = "candidate-row", `data-candidate` = i,
          tags$span(class = "candidate-id", LETTERS[i]), heat_row(ch$candidates[i, ]))
      }))
    )
  })
  observeEvent(input$curse_pick, {
    req(!completed$curse, is.null(state$curse_choice)); ensure_round_timer("curse")
    choice <- as.integer(input$curse_pick$choice); ch <- curse()
    state$curse_choice <- choice; state$curse_result <- list(
      points = curse_points(ch$distances, choice), raw = ch$distances[choice], optimum = min(ch$distances),
      choice = choice, nearest = ch$nearest, contrast = ch$contrast, distances = ch$distances
    )
    shinyjs_toggle_button(session, "curse_submit", TRUE)
    session$sendCustomMessage("curseAnswer", list(choice = choice, nearest = ch$nearest))
  })
  output$curse_feedback <- renderUI({
    r <- state$curse_result
    if (is.null(r)) return(tags$p(class = "curse-prompt", "Selecciona A–F usando todas las columnas. Los colores exactos cambian en la siguiente ronda."))
    tags$div(class = "curse-feedback",
      tags$div(class = if (r$choice == r$nearest) "feedback-verdict correct" else "feedback-verdict",
        if (r$choice == r$nearest) "¡Era el vecino más próximo!" else sprintf("El más próximo era %s", LETTERS[r$nearest])),
      tags$div(class = "distance-bars", lapply(seq_along(r$distances), function(i) tags$div(class = "distance-row",
        tags$span(LETTERS[i]), tags$i(style = sprintf("width: %.1f%%", 100 * r$distances[i] / max(r$distances))), tags$small(sprintf("%.3f", r$distances[i]))))),
      tags$p(sprintf("Contraste relativo: %.1f%%. Cierra la ronda para avanzar a la siguiente dimensión.", 100 * r$contrast))
    )
  })
  output$curse_score <- renderUI(if (is.null(state$curse_result)) score_ui() else score_ui(state$curse_result$points, state$curse_result$raw, state$curse_result$optimum, "distancia elegida", "curse", state$curse_result$base_points %||% state$curse_result$points, !is.null(state$curse_result$base_points)))

  current_result <- list(
    mean = function() state$mean_result, kmeans = function() state$kmeans_result,
    pca = function() state$pca_result, regression = function() state$regression_result,
    shadows = function() state$shadow_result, curse = function() state$curse_result
  )

  set_result_points <- function(game, points, base_points = points) {
    key <- c(mean = "mean_result", kmeans = "kmeans_result", pca = "pca_result",
             regression = "regression_result", shadows = "shadow_result", curse = "curse_result")[[game]]
    result <- state[[key]]
    result$points <- as.integer(points)
    result$base_points <- as.integer(base_points)
    state[[key]] <- result
  }

  complete_round <- function(game, result) {
    req(nzchar(identity$player), !is.null(result), !completed[[game]])
    seconds <- elapsed_round(game)
    base_points <- as.integer(result$points)
    final_points <- if (forfeited[[game]]) 0L else apply_time_penalty(base_points, seconds)
    time_lost <- if (forfeited[[game]]) 0L else base_points - final_points
    set_result_points(game, final_points, base_points)
    scores <- session_scores()
    scores <- rbind(scores, data.frame(game = game, round = progress[[game]], points = final_points,
                                      raw = as.numeric(result$raw), seconds = seconds,
                                      base_points = base_points, time_lost = time_lost))
    session_scores(scores)
    if (progress[[game]] < round_totals[[game]]) {
      progress[[game]] <- progress[[game]] + 1L
      reset_game(game, new_round = TRUE)
      start_round_timer(game)
      if (game == "shadows") session$sendCustomMessage("setShadowTarget", list(target = shadow_targets[progress$shadows]))
      showNotification(sprintf("Ronda cerrada: +%d puntos · −%d por tiempo (%.1f s)",
                               final_points, time_lost, seconds), type = "message")
    } else {
      completed[[game]] <- TRUE
      session$sendCustomMessage("stopGameTimer", list(game = game))
      shinyjs_toggle_button(session, submit_ids[[game]], FALSE)
      total <- sum(scores$points[scores$game == game])
      updateActionButton(session, submit_ids[[game]], label = sprintf("Serie completada · %s pts", formatC(total, format = "d", big.mark = ".", decimal.mark = ",")))
      showNotification(sprintf("%s completado: %d puntos", game_names[[game]], total), type = "message", duration = 7)
    }
  }

  lapply(names(submit_ids), function(game) local({
    id <- submit_ids[[game]]
    observeEvent(input[[id]], complete_round(game, current_result[[game]]()), ignoreInit = TRUE)
  }))

  output$session_scoreboard <- renderUI({
    scores <- session_scores(); total <- sum(scores$points); max_total <- sum(round_totals) * 1000L
    tags$div(class = "session-board",
      tags$div(class = "session-player", tags$span("JUGADOR"), tags$strong(identity$player %||% "—")),
      tags$div(class = "overall-score", tags$span(formatC(total, format = "d", big.mark = ".", decimal.mark = ",")), tags$small(sprintf("/ %s", formatC(max_total, format = "d", big.mark = ".", decimal.mark = ",")))),
      tags$div(class = "rank-list", lapply(names(round_totals), function(game) {
        rows <- scores[scores$game == game, , drop = FALSE]; game_total <- sum(rows$points)
        tags$div(class = "rank-row",
          tags$span(class = "rank-position", game_icons[[game]]),
          tags$div(class = "rank-person", tags$strong(game_names[[game]]), tags$small(sprintf("%d/%d rondas", nrow(rows), round_totals[[game]]))),
          tags$span(class = "rank-score", formatC(game_total, format = "d", big.mark = ".", decimal.mark = ","))
        )
      }))
    )
  })
}

shinyjs_toggle_button <- function(session, id, enabled) {
  session$sendCustomMessage("toggleButton", list(id = id, enabled = isTRUE(enabled)))
}

shinyApp(ui, server)
