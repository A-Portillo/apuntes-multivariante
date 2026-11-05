# Funciones sencillas para el tema de correspondencias multiples
#
# Requisitos:
# - cargar tidyverse antes de usar las funciones;
# - representar individuos en filas y variables categoricas en columnas;
# - guardar las variables categoricas como factores y eliminar niveles vacios;
# - decidir antes del ajuste que variables seran activas y suplementarias;
# - ajustar el modelo con FactoMineR::MCA(datos, graph = FALSE);
# - conservar en el ajuste tantas dimensiones como se quieran interpretar.
#
# Las funciones hacen pocos chequeos de forma intencionada. La eleccion de
# variables activas, la agrupacion de categorias raras y la interpretacion
# sustantiva de las dimensiones siguen correspondiendo al analista.


crear_indicadora_mca <- function(datos) {
  bloques <- lapply(names(datos), function(variable) {
    x <- datos[[variable]]
    if (!is.factor(x)) x <- factor(x)
    bloque <- stats::model.matrix(~ x - 1)
    colnames(bloque) <- paste(variable, levels(x), sep = ": ")
    bloque
  })

  indicadora <- do.call(cbind, bloques)
  indicadora[, colSums(indicadora) > 0, drop = FALSE]
}


crear_burt_mca <- function(datos) {
  indicadora <- crear_indicadora_mca(datos)
  t(indicadora) %*% indicadora
}


.mapa_modalidades_mca <- function(ajuste, suplementarias = FALSE) {
  if (suplementarias) {
    indices <- ajuste$call$quali.sup
    elemento <- ajuste$quali.sup
  } else {
    indices <- ajuste$call$quali
    elemento <- ajuste$var
  }

  datos <- ajuste$call$X[, indices, drop = FALSE]
  mapa <- dplyr::bind_rows(lapply(names(datos), function(variable) {
    niveles <- levels(datos[[variable]])
    tibble::tibble(
      variable = variable,
      modalidad = niveles,
      etiqueta = paste(variable, niveles, sep = ": ")
    )
  }))

  mapa$orden <- seq_len(nrow(mapa))
  mapa$nombre_factominer <- make.unique(rownames(elemento$coord))
  mapa
}


.preparar_elemento_mca <- function(elemento, identificador, tipo) {
  coord_matriz <- elemento$coord
  cos2_matriz <- elemento$cos2
  identificadores <- make.unique(rownames(coord_matriz))
  rownames(coord_matriz) <- identificadores
  rownames(cos2_matriz) <- identificadores

  coord <- tibble::as_tibble(coord_matriz, rownames = identificador) |>
    tidyr::pivot_longer(-dplyr::all_of(identificador), names_to = "dimension", values_to = "coordenada")
  cos2 <- tibble::as_tibble(cos2_matriz, rownames = identificador) |>
    tidyr::pivot_longer(-dplyr::all_of(identificador), names_to = "dimension", values_to = "cos2")

  resultado <- coord |>
    dplyr::left_join(cos2, by = c(identificador, "dimension")) |>
    dplyr::mutate(tipo = tipo)

  if (!is.null(elemento$contrib)) {
    contrib_matriz <- elemento$contrib
    rownames(contrib_matriz) <- identificadores
    contrib <- tibble::as_tibble(contrib_matriz, rownames = identificador) |>
      tidyr::pivot_longer(-dplyr::all_of(identificador), names_to = "dimension", values_to = "contribucion")
    resultado <- resultado |>
      dplyr::left_join(contrib, by = c(identificador, "dimension"))
  }
  resultado
}


resumir_inercia_mca <- function(
  ajuste,
  correccion = c("bruta", "benzecri")
) {
  correccion <- match.arg(correccion)
  lambda <- ajuste$eig[, 1]
  Q <- length(ajuste$call$quali)

  if (correccion == "bruta") {
    porcentaje <- ajuste$eig[, 2]
  } else {
    lambda <- ifelse(
      lambda > 1 / Q,
      (Q / (Q - 1))^2 * (lambda - 1 / Q)^2,
      0
    )
    total <- sum(lambda)
    porcentaje <- if (total > 0) 100 * lambda / total else rep(0, length(lambda))
  }

  tibble::tibble(
    dimension = seq_along(lambda),
    autovalor = as.numeric(lambda),
    porcentaje = as.numeric(porcentaje),
    acumulado = cumsum(porcentaje),
    correccion = correccion
  )
}


resumir_mca <- function(
  ajuste,
  correccion = c("bruta", "benzecri")
) {
  correccion <- match.arg(correccion)
  mapa <- .mapa_modalidades_mca(ajuste)

  modalidades <- .preparar_elemento_mca(ajuste$var, "nombre_factominer", "Modalidad") |>
    dplyr::left_join(mapa, by = "nombre_factominer")

  individuos <- .preparar_elemento_mca(ajuste$ind, "individuo", "Individuo")

  variables <- tibble::as_tibble(ajuste$var$eta2, rownames = "variable") |>
    tidyr::pivot_longer(-variable, names_to = "dimension", values_to = "eta2")

  resultado <- list(
    autovalores = resumir_inercia_mca(ajuste, correccion),
    modalidades = modalidades,
    variables = variables,
    individuos = individuos
  )

  if (!is.null(ajuste$quali.sup)) {
    mapa_sup <- .mapa_modalidades_mca(ajuste, suplementarias = TRUE)
    resultado$modalidades_suplementarias <-
      .preparar_elemento_mca(
        ajuste$quali.sup,
        "nombre_factominer",
        "Modalidad suplementaria"
      ) |>
      dplyr::left_join(mapa_sup, by = "nombre_factominer")

    resultado$variables_suplementarias <-
      tibble::as_tibble(ajuste$quali.sup$eta2, rownames = "variable") |>
      tidyr::pivot_longer(-variable, names_to = "dimension", values_to = "eta2")
  }

  resultado
}


visualizar_inercia_mca <- function(
  ajuste,
  correccion = c("bruta", "benzecri")
) {
  correccion <- match.arg(correccion)
  tabla <- resumir_inercia_mca(ajuste, correccion)
  Q <- length(ajuste$call$quali)
  nombre_correccion <- if (correccion == "bruta") {
    "bruta"
  } else {
    "corregida de Benzécri"
  }

  grafico <- ggplot2::ggplot(tabla, ggplot2::aes(dimension, porcentaje)) +
    ggplot2::geom_col(fill = "#2c7fb8", alpha = 0.85) +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "gray25") +
    ggplot2::geom_point(color = "gray25") +
    ggplot2::scale_x_continuous(breaks = tabla$dimension) +
    ggplot2::labs(
      title = paste("Inercia", nombre_correccion, "en MCA"),
      x = "Dimensión", y = "Porcentaje de inercia"
    ) +
    ggplot2::theme_minimal()

  if (correccion == "bruta") {
    inercia_total <- ajuste$eig[1, 1] / (ajuste$eig[1, 2] / 100)
    umbral <- 100 * (1 / Q) / inercia_total
    grafico <- grafico + ggplot2::geom_hline(
      yintercept = umbral,
      linetype = "dashed",
      color = "#d95f0e"
    )
  }
  grafico
}


visualizar_mca <- function(
  ajuste,
  tipo = c("modalidades", "individuos"),
  ejes = c(1, 2),
  grupo = NULL,
  etiquetas = TRUE
) {
  tipo <- match.arg(tipo)
  nombres_dim <- colnames(ajuste$ind$coord)[ejes]

  if (tipo == "individuos") {
    datos <- tibble::as_tibble(
      ajuste$ind$coord[, ejes, drop = FALSE],
      rownames = "individuo"
    )
    datos$grupo <- if (is.null(grupo)) factor("Individuos") else factor(grupo)

    return(
      ggplot2::ggplot(
        datos,
        ggplot2::aes(
          x = .data[[nombres_dim[1]]],
          y = .data[[nombres_dim[2]]],
          color = grupo
        )
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
        ggplot2::geom_point(alpha = 0.45, size = 1.5) +
        ggplot2::coord_equal() +
        ggplot2::labs(
          title = "Individuos en el espacio MCA",
          x = nombres_dim[1], y = nombres_dim[2], color = NULL
        ) +
        ggplot2::theme_minimal()
    )
  }

  mapa <- .mapa_modalidades_mca(ajuste)
  datos <- tibble::as_tibble(
    ajuste$var$coord[, ejes, drop = FALSE],
    rownames = "nombre_factominer"
  )
  datos$nombre_factominer <- make.unique(datos$nombre_factominer)
  datos <- datos |>
    dplyr::left_join(mapa, by = "nombre_factominer")

  grafico <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = .data[[nombres_dim[1]]],
      y = .data[[nombres_dim[2]]],
      color = variable,
      label = etiqueta
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Modalidades activas en el espacio MCA",
      x = nombres_dim[1], y = nombres_dim[2], color = "Variable"
    ) +
    ggplot2::theme_minimal()

  if (etiquetas) {
    grafico <- grafico + ggplot2::geom_text(
      vjust = -0.7,
      size = 3,
      check_overlap = TRUE,
      show.legend = FALSE
    )
  }
  grafico
}


resumir_dimension_mca <- function(ajuste, dimension = 1, n = 10) {
  nombre_dimension <- colnames(ajuste$var$coord)[dimension]
  mapa <- .mapa_modalidades_mca(ajuste)

  modalidades <- tibble::tibble(
    nombre_factominer = make.unique(rownames(ajuste$var$coord)),
    coordenada = ajuste$var$coord[, dimension],
    contribucion = ajuste$var$contrib[, dimension],
    cos2 = ajuste$var$cos2[, dimension]
  ) |>
    dplyr::left_join(mapa, by = "nombre_factominer") |>
    dplyr::arrange(dplyr::desc(contribucion)) |>
    utils::head(n)

  variables <- tibble::tibble(
    variable = rownames(ajuste$var$eta2),
    eta2 = ajuste$var$eta2[, dimension]
  ) |>
    dplyr::arrange(dplyr::desc(eta2))

  list(
    dimension = nombre_dimension,
    modalidades = modalidades,
    variables = variables
  )
}


visualizar_contribuciones_mca <- function(
  ajuste,
  dimension = 1,
  n = 10
) {
  tabla <- resumir_dimension_mca(ajuste, dimension, n)$modalidades

  ggplot2::ggplot(
    tabla,
    ggplot2::aes(stats::reorder(etiqueta, contribucion), contribucion, fill = variable)
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Modalidades que construyen la dimensión", dimension),
      x = NULL, y = "Contribución (%)"
    ) +
    ggplot2::theme_minimal()
}


calidad_mca <- function(ajuste, ejes = c(1, 2)) {
  mapa <- .mapa_modalidades_mca(ajuste)
  cos2 <- rowSums(ajuste$var$cos2[, ejes, drop = FALSE])

  tabla <- tibble::tibble(
    nombre_factominer = make.unique(rownames(ajuste$var$cos2)),
    cos2_plano = as.numeric(cos2)
  ) |>
    dplyr::left_join(mapa, by = "nombre_factominer") |>
    dplyr::arrange(cos2_plano)

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(stats::reorder(etiqueta, cos2_plano), cos2_plano, fill = variable)
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = "Calidad de representación de las modalidades",
      x = NULL, y = "Cos² acumulado en el plano"
    ) +
    ggplot2::theme_minimal()

  list(tabla = tabla, grafico = grafico)
}


visualizar_discriminacion_mca <- function(ajuste, ejes = c(1, 2)) {
  nombres_dim <- colnames(ajuste$var$eta2)[ejes]
  datos <- tibble::as_tibble(
    ajuste$var$eta2[, ejes, drop = FALSE],
    rownames = "variable"
  ) |>
    tidyr::pivot_longer(-variable, names_to = "dimension", values_to = "eta2") |>
    dplyr::mutate(dimension = factor(dimension, levels = nombres_dim))

  ggplot2::ggplot(datos, ggplot2::aes(dimension, variable, fill = eta2)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", eta2))) +
    ggplot2::scale_fill_viridis_c(option = "C", limits = c(0, 1)) +
    ggplot2::labs(
      title = "Capacidad discriminante de las variables",
      x = NULL, y = NULL, fill = expression(eta^2)
    ) +
    ggplot2::theme_minimal()
}
