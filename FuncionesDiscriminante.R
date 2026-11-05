# Funciones sencillas para el tema de analisis discriminante
#
# Requisitos:
# - cargar tidyverse y tener instalado MASS;
# - representar individuos en filas, predictores numericos en columnas y la
#   clase conocida en una columna factor;
# - separar entrenamiento y test antes de ajustar o transformar los datos;
# - usar los mismos niveles de clase en entrenamiento y test;
# - ajustar LDA o QDA con MASS::lda() y MASS::qda().
#
# Las funciones hacen pocos chequeos de forma intencionada. La eleccion de
# predictores, priors, costes, esquema de validacion y consecuencias de los
# errores siguen correspondiendo al analista.


particionar_clasificacion <- function(
  datos,
  respuesta,
  proporcion = 0.70,
  semilla = 123
) {
  set.seed(semilla)
  grupos <- split(seq_len(nrow(datos)), datos[[respuesta]])
  indices_entrenamiento <- unlist(lapply(grupos, function(indices) {
    cantidad <- floor(proporcion * length(indices))
    sample(indices, cantidad)
  }), use.names = FALSE)

  list(
    entrenamiento = datos[indices_entrenamiento, , drop = FALSE],
    test = datos[-indices_entrenamiento, , drop = FALSE],
    indices_entrenamiento = indices_entrenamiento
  )
}


evaluar_clasificacion <- function(observado, predicho) {
  niveles <- union(levels(factor(observado)), levels(factor(predicho)))
  observado <- factor(observado, levels = niveles)
  predicho <- factor(predicho, levels = niveles)
  matriz <- table(Observado = observado, Predicho = predicho)

  total <- sum(matriz)
  tp <- diag(matriz)
  fn <- rowSums(matriz) - tp
  fp <- colSums(matriz) - tp
  tn <- total - tp - fn - fp

  sensibilidad <- tp / (tp + fn)
  especificidad <- tn / (tn + fp)
  precision <- tp / (tp + fp)
  f1 <- 2 * precision * sensibilidad / (precision + sensibilidad)

  por_clase <- tibble::tibble(
    clase = niveles,
    soporte = as.numeric(rowSums(matriz)),
    sensibilidad = as.numeric(sensibilidad),
    especificidad = as.numeric(especificidad),
    precision = as.numeric(precision),
    f1 = as.numeric(f1)
  )

  global <- tibble::tibble(
    accuracy = sum(tp) / total,
    balanced_accuracy = mean(sensibilidad, na.rm = TRUE),
    macro_f1 = mean(f1, na.rm = TRUE)
  )

  list(matriz = matriz, global = global, por_clase = por_clase)
}


visualizar_matriz_confusion <- function(
  evaluacion,
  mostrar = c("conteos", "porcentaje_fila", "porcentaje_total"),
  mostrar_numeros = TRUE
) {
  mostrar <- match.arg(mostrar)
  matriz <- evaluacion$matriz
  valores <- switch(
    mostrar,
    conteos = matriz,
    porcentaje_fila = 100 * prop.table(matriz, 1),
    porcentaje_total = 100 * prop.table(matriz)
  )
  etiqueta <- if (mostrar == "conteos") "%.0f" else "%.1f"

  datos <- tibble::as_tibble(as.data.frame.matrix(valores), rownames = "observado") |>
    tidyr::pivot_longer(-observado, names_to = "predicho", values_to = "valor") |>
    dplyr::mutate(
      predicho = factor(predicho, levels = colnames(matriz)),
      observado = factor(observado, levels = rev(rownames(matriz)))
    )

  grafico <- ggplot2::ggplot(datos, ggplot2::aes(predicho, observado, fill = valor)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_viridis_c(option = "C") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Matriz de confusión",
      x = "Clase predicha", y = "Clase observada",
      fill = if (mostrar == "conteos") "Casos" else "%"
    ) +
    ggplot2::theme_minimal()

  if (mostrar_numeros) {
    grafico <- grafico + ggplot2::geom_text(
      ggplot2::aes(label = sprintf(etiqueta, valor))
    )
  }
  grafico
}


comparar_modelos_discriminantes <- function(
  modelos,
  datos_test,
  respuesta
) {
  resultados <- lapply(names(modelos), function(nombre) {
    prediccion <- stats::predict(modelos[[nombre]], newdata = datos_test)
    evaluacion <- evaluar_clasificacion(datos_test[[respuesta]], prediccion$class)
    list(
      nombre = nombre,
      prediccion = prediccion,
      evaluacion = evaluacion
    )
  })
  names(resultados) <- names(modelos)

  metricas <- dplyr::bind_rows(lapply(resultados, function(x) {
    dplyr::mutate(x$evaluacion$global, modelo = x$nombre, .before = 1)
  }))
  metricas_clase <- dplyr::bind_rows(lapply(resultados, function(x) {
    dplyr::mutate(x$evaluacion$por_clase, modelo = x$nombre, .before = 1)
  }))

  list(
    metricas = metricas,
    metricas_clase = metricas_clase,
    resultados = resultados
  )
}


resumir_predicciones_discriminantes <- function(
  prediccion,
  observado = NULL,
  id = NULL
) {
  posteriores <- as.matrix(prediccion$posterior)
  ordenadas <- t(apply(posteriores, 1, sort, decreasing = TRUE))
  if (is.null(id)) id <- seq_len(nrow(posteriores))

  resultado <- tibble::tibble(
    id = id,
    predicho = prediccion$class,
    probabilidad_maxima = apply(posteriores, 1, max),
    margen = ordenadas[, 1] - ordenadas[, 2]
  )
  if (!is.null(observado)) {
    resultado <- resultado |>
      dplyr::mutate(
        observado = observado,
        acierto = predicho == observado
      )
  }
  dplyr::arrange(resultado, margen)
}


resumir_lda <- function(ajuste) {
  autovalores <- ajuste$svd^2
  ejes <- tibble::tibble(
    eje = paste0("LD", seq_along(autovalores)),
    autovalor = autovalores,
    proporcion_separacion = autovalores / sum(autovalores),
    acumulado = cumsum(autovalores / sum(autovalores))
  )

  list(
    priors = tibble::tibble(clase = names(ajuste$prior), prior = ajuste$prior),
    medias = tibble::as_tibble(ajuste$means, rownames = "clase"),
    coeficientes = tibble::as_tibble(ajuste$scaling, rownames = "variable"),
    ejes = ejes
  )
}


visualizar_ejes_discriminantes <- function(
  ajuste,
  datos,
  respuesta,
  etiquetas = NULL
) {
  coordenadas <- as.data.frame(stats::predict(ajuste, newdata = datos)$x)
  coordenadas$clase <- datos[[respuesta]]
  coordenadas$etiqueta <- if (is.null(etiquetas)) NA_character_ else etiquetas

  if (ncol(coordenadas) == 3) {
    grafico <- ggplot2::ggplot(
      coordenadas,
      ggplot2::aes(LD1, 0, color = clase)
    ) +
      ggplot2::geom_jitter(height = 0.05, width = 0, size = 2, alpha = 0.75) +
      ggplot2::labs(y = NULL)
  } else {
    grafico <- ggplot2::ggplot(
      coordenadas,
      ggplot2::aes(LD1, LD2, color = clase)
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray75") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray75") +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggplot2::coord_equal()
  }

  grafico <- grafico +
    ggplot2::labs(
      title = "Ejes canónicos discriminantes",
      color = "Clase"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(etiquetas)) {
    grafico <- grafico + ggplot2::geom_text(
      ggplot2::aes(label = etiqueta),
      vjust = -0.7,
      check_overlap = TRUE,
      show.legend = FALSE
    )
  }
  grafico
}


visualizar_fronteras_discriminantes <- function(
  ajuste,
  datos,
  respuesta,
  variables,
  puntos = 180
) {
  rangos <- lapply(datos[variables], range)
  rejilla <- expand.grid(
    seq(rangos[[1]][1], rangos[[1]][2], length.out = puntos),
    seq(rangos[[2]][1], rangos[[2]][2], length.out = puntos)
  )
  names(rejilla) <- variables
  rejilla$clase <- stats::predict(ajuste, newdata = rejilla)$class

  ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = rejilla,
      ggplot2::aes(
        x = .data[[variables[1]]],
        y = .data[[variables[2]]],
        fill = clase
      ),
      alpha = 0.28
    ) +
    ggplot2::geom_point(
      data = datos,
      ggplot2::aes(
        x = .data[[variables[1]]],
        y = .data[[variables[2]]],
        color = .data[[respuesta]]
      ),
      size = 2,
      alpha = 0.8
    ) +
    ggplot2::labs(
      title = "Regiones de decisión",
      x = variables[1], y = variables[2],
      fill = "Predicción", color = "Observado"
    ) +
    ggplot2::theme_minimal()
}


visualizar_covarianzas_grupos <- function(
  datos,
  respuesta,
  variables,
  estandarizar = TRUE
) {
  X <- as.data.frame(datos[variables])
  if (estandarizar) X <- as.data.frame(scale(X))
  X[[respuesta]] <- datos[[respuesta]]

  matrices <- lapply(split(X[variables], X[[respuesta]]), stats::cov)
  tabla <- dplyr::bind_rows(lapply(names(matrices), function(grupo) {
    tibble::as_tibble(matrices[[grupo]], rownames = "variable_1") |>
      tidyr::pivot_longer(-variable_1, names_to = "variable_2", values_to = "covarianza") |>
      dplyr::mutate(grupo = grupo)
  })) |>
    dplyr::mutate(
      variable_1 = factor(variable_1, levels = rev(variables)),
      variable_2 = factor(variable_2, levels = variables)
    )
  limite <- max(abs(tabla$covarianza))

  ggplot2::ggplot(tabla, ggplot2::aes(variable_1, variable_2, fill = covarianza)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac",
      midpoint = 0, limits = c(-limite, limite)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(grupo)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Covarianzas dentro de cada clase",
      subtitle = if (estandarizar) "Predictores estandarizados globalmente" else NULL,
      x = NULL, y = NULL, fill = "Covarianza"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


clasificar_con_costes <- function(posteriores, matriz_costes) {
  posteriores <- as.matrix(posteriores)
  matriz_costes <- as.matrix(matriz_costes)
  costes_esperados <- posteriores[, rownames(matriz_costes), drop = FALSE] %*%
    matriz_costes
  decisiones <- colnames(matriz_costes)[max.col(-costes_esperados)]
  factor(decisiones, levels = colnames(matriz_costes))
}


validacion_cruzada_discriminante <- function(
  datos,
  respuesta,
  metodo = c("lda", "qda"),
  k = 5,
  semilla = 123
) {
  metodo <- match.arg(metodo)
  set.seed(semilla)

  pliegues <- integer(nrow(datos))
  grupos <- split(seq_len(nrow(datos)), datos[[respuesta]])
  for (indices in grupos) {
    pliegues[indices] <- sample(rep(seq_len(k), length.out = length(indices)))
  }

  predicho <- factor(rep(NA_character_, nrow(datos)), levels = levels(datos[[respuesta]]))
  formula <- stats::as.formula(paste(respuesta, "~ ."))

  for (pliegue in seq_len(k)) {
    entrenamiento <- datos[pliegues != pliegue, , drop = FALSE]
    validacion <- datos[pliegues == pliegue, , drop = FALSE]
    ajuste <- if (metodo == "lda") {
      MASS::lda(formula, data = entrenamiento)
    } else {
      MASS::qda(formula, data = entrenamiento)
    }
    predicho[pliegues == pliegue] <- stats::predict(ajuste, validacion)$class
  }

  evaluacion <- evaluar_clasificacion(datos[[respuesta]], predicho)
  list(
    metodo = metodo,
    k = k,
    predicho = predicho,
    pliegue = pliegues,
    evaluacion = evaluacion
  )
}
