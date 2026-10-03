# Funciones sencillas para el tema de componentes principales
#
# Requisitos:
# - cargar tidyverse antes de usar las funciones;
# - usar una tabla o matriz con individuos en filas y variables numericas en columnas;
# - ajustar el PCA con prcomp(..., center = TRUE, scale. = TRUE/FALSE);
# - conservar en el objeto de prcomp los scores (x), rotation, center y scale;
# - no incluir valores perdidos ni variables constantes;
# - indicar componentes que existan en el ajuste.
# Las variables suplementarias deben ser numericas y tener las mismas filas
# que los datos usados para ajustar el PCA.
#
# Las comprobaciones son deliberadamente breves para que el codigo sea legible.

.datos_escala_pca <- function(datos, ajuste) {
  datos <- as.matrix(datos)
  centro <- if (is.numeric(ajuste$center)) ajuste$center else FALSE
  escala <- if (is.numeric(ajuste$scale)) ajuste$scale else FALSE
  scale(datos, center = centro, scale = escala)
}

visualizar_direcciones_2d <- function(datos, direcciones) {
  datos <- as.data.frame(datos[, 1:2, drop = FALSE])
  nombres <- colnames(datos)
  direcciones <- as.matrix(direcciones)
  direcciones <- sweep(
    direcciones,
    2,
    sqrt(colSums(direcciones^2)),
    "/"
  )
  longitud <- 0.4 * max(abs(as.matrix(datos)))
  nombres_direcciones <- colnames(direcciones)
  if (is.null(nombres_direcciones)) {
    nombres_direcciones <- paste0("Direccion ", seq_len(ncol(direcciones)))
  }
  ejes <- tibble::tibble(
    componente = nombres_direcciones,
    x = direcciones[1, ] * longitud,
    y = direcciones[2, ] * longitud
  )

  ggplot2::ggplot(datos, ggplot2::aes(x = .data[[nombres[1]]], y = .data[[nombres[2]]])) +
    ggplot2::geom_point(color = "gray40", alpha = 0.75) +
    ggplot2::geom_segment(
      data = ejes,
      ggplot2::aes(x = 0, y = 0, xend = x, yend = y, color = componente),
      arrow = grid::arrow(length = grid::unit(0.18, "cm")),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = nombres[1], y = nombres[2], color = "Direccion") +
    ggplot2::theme_minimal()
}

preparar_proyeccion <- function(datos, direccion, nombre) {
  datos <- as.matrix(datos[, 1:2, drop = FALSE])
  direccion <- direccion / sqrt(sum(direccion^2))
  scores <- drop(datos %*% direccion)
  reconstruccion <- tcrossprod(scores, direccion)
  error <- sum((datos - reconstruccion)^2)

  tibble::tibble(
    x1 = datos[, 1],
    x2 = datos[, 2],
    x1_proy = reconstruccion[, 1],
    x2_proy = reconstruccion[, 2],
    direccion = paste0(
      nombre,
      "\nVarianza = ", round(stats::var(scores), 2),
      "; error = ", round(error, 1)
    )
  )
}

resumir_varianza_pca <- function(ajuste, umbrales = c(0.70, 0.80, 0.90, 0.95)) {
  autovalores <- ajuste$sdev^2
  proporcion <- autovalores / sum(autovalores)
  componentes <- factor(
    paste0("PC", seq_along(autovalores)),
    levels = paste0("PC", seq_along(autovalores))
  )
  estandarizado <- is.numeric(ajuste$scale)

  tabla <- tibble::tibble(
    componente = componentes,
    autovalor = autovalores,
    proporcion = proporcion,
    acumulada = cumsum(proporcion),
    kaiser = if (estandarizado) autovalores > 1 else NA
  )
  tabla_umbrales <- tibble::tibble(
    umbral = umbrales,
    componentes = vapply(
      umbrales,
      function(x) which(cumsum(proporcion) >= x)[1],
      integer(1)
    )
  )

  grafico_scree <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(componente, autovalor, group = 1)
  ) +
    ggplot2::geom_col(fill = "#4c78a8", alpha = 0.8) +
    ggplot2::geom_line(color = "#222222") +
    ggplot2::geom_point(size = 2) +
    {if (estandarizado) ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#d95f0e")} +
    ggplot2::labs(
      title = "Scree plot",
      x = "Componente", y = "Autovalor"
    ) +
    ggplot2::theme_minimal()

  grafico_acumulada <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(componente, acumulada, group = 1)
  ) +
    ggplot2::geom_line(color = "#2c7fb8") +
    ggplot2::geom_point(color = "#2c7fb8", size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
    ggplot2::labs(
      title = "Varianza explicada acumulada",
      x = "Componente", y = "Proporcion acumulada"
    ) +
    ggplot2::theme_minimal()

  list(
    tabla = tabla,
    umbrales = tabla_umbrales,
    grafico_scree = grafico_scree,
    grafico_acumulada = grafico_acumulada
  )
}

resumir_variables_pca <- function(ajuste, datos, componentes = seq_len(ncol(ajuste$rotation))) {
  correlaciones <- cor(as.matrix(datos), ajuste$x)
  nombres_componentes <- colnames(ajuste$rotation)[componentes]

  tibble::tibble(
    variable = rep(rownames(ajuste$rotation), times = length(componentes)),
    componente = rep(nombres_componentes, each = nrow(ajuste$rotation)),
    coeficiente = as.vector(ajuste$rotation[, componentes, drop = FALSE]),
    correlacion = as.vector(correlaciones[, componentes, drop = FALSE])
  ) |>
    dplyr::mutate(
      cos2 = correlacion^2,
      contribucion = 100 * coeficiente^2
    )
}

circulo_correlaciones_pca <- function(ajuste, datos, componentes = c(1, 2), etiquetas = NULL) {
  resumen <- resumir_variables_pca(ajuste, datos, componentes) |>
    dplyr::select(variable, componente, correlacion) |>
    tidyr::pivot_wider(names_from = componente, values_from = correlacion)
  nombres_componentes <- colnames(ajuste$rotation)[componentes]
  if (is.null(etiquetas)) etiquetas <- resumen$variable
  resumen$etiqueta <- etiquetas
  circulo <- tibble::tibble(
    x = cos(seq(0, 2 * pi, length.out = 361)),
    y = sin(seq(0, 2 * pi, length.out = 361))
  )
  porcentajes <- 100 * ajuste$sdev^2 / sum(ajuste$sdev^2)

  ggplot2::ggplot() +
    ggplot2::geom_path(data = circulo, ggplot2::aes(x, y), color = "gray55") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_segment(
      data = resumen,
      ggplot2::aes(
        x = 0, y = 0,
        xend = .data[[nombres_componentes[1]]],
        yend = .data[[nombres_componentes[2]]]
      ),
      arrow = grid::arrow(length = grid::unit(0.14, "cm")),
      color = "#2c7fb8", linewidth = 0.8
    ) +
    ggplot2::geom_text(
      data = resumen,
      ggplot2::aes(
        x = .data[[nombres_componentes[1]]],
        y = .data[[nombres_componentes[2]]],
        label = etiqueta
      ),
      nudge_y = 0.045, size = 3.5
    ) +
    ggplot2::coord_equal(xlim = c(-1.08, 1.08), ylim = c(-1.08, 1.08)) +
    ggplot2::labs(
      title = "Circulo de correlaciones",
      x = paste0(nombres_componentes[1], " (", round(porcentajes[componentes[1]], 1), " %)") ,
      y = paste0(nombres_componentes[2], " (", round(porcentajes[componentes[2]], 1), " %)")
    ) +
    ggplot2::theme_minimal()
}

resumir_individuos_pca <- function(ajuste, componentes = c(1, 2), id = NULL) {
  scores <- ajuste$x
  if (is.null(id)) {
    id <- rownames(scores)
    if (is.null(id)) id <- seq_len(nrow(scores))
  }
  distancia_cuadrada <- rowSums(scores^2)
  cos2 <- scores[, componentes, drop = FALSE]^2 / distancia_cuadrada
  cos2[!is.finite(cos2)] <- 0

  resultado <- tibble::as_tibble(scores[, componentes, drop = FALSE])
  resultado <- dplyr::bind_cols(
    tibble::tibble(id = id),
    resultado,
    tibble::as_tibble(cos2, .name_repair = ~ paste0("cos2_", .x))
  )
  resultado$distancia_cuadrada <- distancia_cuadrada
  resultado$cos2_plano <- rowSums(cos2)
  resultado
}

visualizar_individuos_pca <- function(
    ajuste, componentes = c(1, 2), grupo = NULL, id = NULL,
    mostrar_etiquetas = FALSE, mostrar_cos2 = FALSE) {
  datos <- resumir_individuos_pca(ajuste, componentes, id)
  nombres_componentes <- colnames(ajuste$x)[componentes]
  porcentajes <- 100 * ajuste$sdev^2 / sum(ajuste$sdev^2)
  datos$grupo <- if (is.null(grupo)) factor("Individuos") else grupo

  grafico <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = .data[[nombres_componentes[1]]],
      y = .data[[nombres_componentes[2]]],
      color = grupo
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, color = "gray80") +
    ggplot2::geom_vline(xintercept = 0, color = "gray80")

  if (mostrar_cos2) {
    grafico <- grafico +
      ggplot2::geom_point(ggplot2::aes(size = cos2_plano), alpha = 0.8) +
      ggplot2::scale_size_continuous(range = c(1.5, 5), limits = c(0, 1))
  } else {
    grafico <- grafico + ggplot2::geom_point(size = 2, alpha = 0.85)
  }
  if (mostrar_etiquetas) {
    grafico <- grafico + ggplot2::geom_text(ggplot2::aes(label = id), check_overlap = TRUE, vjust = -0.7)
  }

  grafico +
    ggplot2::labs(
      title = "Individuos en el plano PCA",
      x = paste0(nombres_componentes[1], " (", round(porcentajes[componentes[1]], 1), " %)") ,
      y = paste0(nombres_componentes[2], " (", round(porcentajes[componentes[2]], 1), " %)") ,
      color = if (is.null(grupo)) NULL else "Grupo",
      size = "Cos2"
    ) +
    ggplot2::theme_minimal()
}

analisis_paralelo_pca <- function(datos, simulaciones = 500, percentil = 0.95, semilla = 123) {
  datos <- as.matrix(datos)
  set.seed(semilla)
  observados <- prcomp(datos, center = TRUE, scale. = TRUE)$sdev^2
  aleatorios <- replicate(
    simulaciones,
    prcomp(
      matrix(rnorm(nrow(datos) * ncol(datos)), nrow = nrow(datos)),
      center = TRUE, scale. = TRUE
    )$sdev^2
  )
  referencia <- apply(aleatorios, 1, stats::quantile, probs = percentil)
  tabla <- tibble::tibble(
    componente = factor(
      paste0("PC", seq_along(observados)),
      levels = paste0("PC", seq_along(observados))
    ),
    observado = observados,
    referencia = referencia,
    supera_referencia = observados > referencia
  )
  grafico <- tabla |>
    dplyr::select(componente, observado, referencia) |>
    tidyr::pivot_longer(-componente, names_to = "serie", values_to = "autovalor") |>
    ggplot2::ggplot(ggplot2::aes(componente, autovalor, color = serie, group = serie)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Scree plot con referencia aleatoria",
      subtitle = paste0("Referencia: percentil ", 100 * percentil, " de ", simulaciones, " simulaciones"),
      x = "Componente", y = "Autovalor", color = NULL
    ) +
    ggplot2::theme_minimal()

  list(
    tabla = tabla,
    grafico = grafico,
    componentes = sum(tabla$supera_referencia)
  )
}

reconstruir_pca <- function(ajuste, q, escala_original = FALSE) {
  componentes <- seq_len(q)
  reconstruccion <- ajuste$x[, componentes, drop = FALSE] %*%
    t(ajuste$rotation[, componentes, drop = FALSE])

  if (escala_original) {
    if (is.numeric(ajuste$scale)) {
      reconstruccion <- sweep(reconstruccion, 2, ajuste$scale, "*")
    }
    if (is.numeric(ajuste$center)) {
      reconstruccion <- sweep(reconstruccion, 2, ajuste$center, "+")
    }
  }
  reconstruccion
}

evaluar_reconstruccion_pca <- function(
    ajuste, datos, q = seq_len(ncol(ajuste$rotation))) {
  datos_pca <- .datos_escala_pca(datos, ajuste)
  p <- ncol(ajuste$rotation)
  lambda <- ajuste$sdev^2

  tabla <- tibble::tibble(
    componentes = q,
    error_empirico = vapply(
      q,
      function(k) sum((datos_pca - reconstruir_pca(ajuste, k))^2),
      numeric(1)
    ),
    error_teorico = vapply(
      q,
      function(k) {
        descartada <- if (k < p) sum(lambda[seq.int(k + 1, p)]) else 0
        (nrow(datos_pca) - 1) * descartada
      },
      numeric(1)
    ),
    varianza_descartada = vapply(
      q,
      function(k) if (k < p) sum(lambda[seq.int(k + 1, p)]) / sum(lambda) else 0,
      numeric(1)
    )
  )
  grafico <- tabla |>
    dplyr::select(componentes, error_empirico, error_teorico) |>
    tidyr::pivot_longer(-componentes, names_to = "calculo", values_to = "error") |>
    ggplot2::ggplot(ggplot2::aes(componentes, error, color = calculo)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = q) +
    ggplot2::labs(
      title = "Error de reconstruccion al anadir componentes",
      x = "Numero de componentes retenidas", y = "Error cuadratico", color = NULL
    ) +
    ggplot2::theme_minimal()

  list(tabla = tabla, grafico = grafico)
}

diagnosticar_reconstruccion_pca <- function(ajuste, datos, q, id = NULL) {
  datos_pca <- .datos_escala_pca(datos, ajuste)
  residuos <- datos_pca - reconstruir_pca(ajuste, q)
  nombres_variables <- colnames(datos_pca)

  resumen_variables <- resumir_variables_pca(ajuste, datos, seq_len(q)) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(cos2_subespacio = sum(cos2), .groups = "drop")
  variables <- tibble::tibble(
    variable = nombres_variables,
    error_reconstruccion = colSums(residuos^2)
  ) |>
    dplyr::left_join(resumen_variables, by = "variable")

  individuos <- resumir_individuos_pca(ajuste, seq_len(q), id) |>
    dplyr::transmute(
      id,
      error_reconstruccion = rowSums(residuos^2),
      cos2_subespacio = cos2_plano
    )

  list(variables = variables, individuos = individuos)
}

evaluar_distancias_pca <- function(ajuste, q = 2, id = NULL) {
  scores <- ajuste$x
  if (is.null(id)) {
    id <- rownames(scores)
    if (is.null(id)) id <- seq_len(nrow(scores))
  }
  pares <- t(utils::combn(seq_len(nrow(scores)), 2))
  distancias_completas <- as.matrix(stats::dist(scores))
  distancias_reducidas <- as.matrix(stats::dist(scores[, seq_len(q), drop = FALSE]))
  indice_pares <- cbind(pares[, 1], pares[, 2])

  tabla <- tibble::tibble(
    individuo_1 = id[pares[, 1]],
    individuo_2 = id[pares[, 2]],
    distancia_completa = distancias_completas[indice_pares],
    distancia_reducida = distancias_reducidas[indice_pares]
  ) |>
    dplyr::mutate(
      perdida_cuadrada = distancia_completa^2 - distancia_reducida^2,
      proporcion_conservada = dplyr::if_else(
        distancia_completa == 0,
        1,
        pmin(1, distancia_reducida / distancia_completa)
      )
    )

  resumen <- tibble::tibble(
    componentes = q,
    correlacion_distancias = stats::cor(tabla$distancia_completa, tabla$distancia_reducida),
    conservacion_media = mean(tabla$proporcion_conservada),
    peor_conservacion = min(tabla$proporcion_conservada)
  )

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(distancia_completa, distancia_reducida, color = proporcion_conservada)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(alpha = 0.65) +
    ggplot2::coord_equal() +
    ggplot2::scale_color_viridis_c(limits = c(0, 1)) +
    ggplot2::labs(
      title = paste0("Distancias conservadas con ", q, " componentes"),
      subtitle = "La diagonal representa conservacion exacta",
      x = "Distancia con todas las componentes",
      y = "Distancia con las componentes retenidas",
      color = "Proporcion\nconservada"
    ) +
    ggplot2::theme_minimal()

  list(tabla = tabla, resumen = resumen, grafico = grafico)
}

resumir_variables_suplementarias_pca <- function(
    ajuste, variables, componentes = c(1, 2)) {
  variables <- as.data.frame(variables)
  correlaciones <- stats::cor(
    as.matrix(variables),
    ajuste$x[, componentes, drop = FALSE]
  )

  tibble::as_tibble(correlaciones, rownames = "variable") |>
    tidyr::pivot_longer(
      -variable,
      names_to = "componente",
      values_to = "correlacion"
    ) |>
    dplyr::mutate(cos2 = correlacion^2)
}
