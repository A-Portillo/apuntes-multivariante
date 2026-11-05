# Funciones sencillas para el tema de FAMD
#
# Requisitos:
# - cargar tidyverse antes de usar las funciones;
# - representar individuos en filas y variables en columnas;
# - guardar las variables categoricas como factores y las cuantitativas como
#   numericas;
# - tratar previamente los valores ausentes y eliminar niveles vacios;
# - ajustar el modelo con FactoMineR::FAMD(datos, graph = FALSE);
# - conservar tantas dimensiones como se quieran interpretar.
#
# Las funciones hacen pocos chequeos de forma intencionada. La seleccion de
# variables, su tipo, las transformaciones previas y la interpretacion
# sustantiva siguen correspondiendo al analista.


transformar_datos_famd <- function(datos) {
  son_numericas <- vapply(datos, is.numeric, logical(1))

  numericas <- lapply(datos[son_numericas], function(x) {
    centrada <- x - mean(x)
    centrada / sqrt(mean(centrada^2))
  })
  matriz_numerica <- as.matrix(as.data.frame(numericas))

  bloques_categoricos <- lapply(names(datos)[!son_numericas], function(variable) {
    x <- datos[[variable]]
    if (!is.factor(x)) x <- factor(x)
    bloque <- stats::model.matrix(~ x - 1)
    proporciones <- colMeans(bloque)
    bloque <- sweep(bloque, 2, proporciones, "-")
    bloque <- sweep(bloque, 2, sqrt(proporciones), "/")
    colnames(bloque) <- paste(variable, levels(x), sep = ": ")
    bloque
  })

  matriz_categorica <- if (length(bloques_categoricos) > 0) {
    do.call(cbind, bloques_categoricos)
  } else {
    NULL
  }

  cbind(matriz_numerica, matriz_categorica)
}


.mapa_modalidades_famd <- function(ajuste, suplementarias = FALSE) {
  naturaleza <- ajuste$call$nature.var
  nombres <- names(ajuste$call$X)
  tipo_buscado <- if (suplementarias) "quali.sup" else "quali"
  variables <- nombres[naturaleza == tipo_buscado]
  elemento <- if (suplementarias) ajuste$quali.sup else ajuste$quali.var

  mapa <- dplyr::bind_rows(lapply(variables, function(variable) {
    niveles <- levels(ajuste$call$X[[variable]])
    tibble::tibble(
      variable = variable,
      modalidad = niveles,
      etiqueta = paste(variable, niveles, sep = ": ")
    )
  }))
  mapa$nombre_factominer <- make.unique(rownames(elemento$coord))
  mapa
}


.elemento_largo_famd <- function(elemento, identificador) {
  identificadores <- make.unique(rownames(elemento$coord))

  preparar <- function(matriz, valor) {
    matriz <- as.matrix(matriz)
    rownames(matriz) <- identificadores
    tibble::as_tibble(matriz, rownames = identificador) |>
      tidyr::pivot_longer(
        -dplyr::all_of(identificador),
        names_to = "dimension",
        values_to = valor
      )
  }

  resultado <- preparar(elemento$coord, "coordenada")
  if (!is.null(elemento$cos2)) {
    resultado <- resultado |>
      dplyr::left_join(
        preparar(elemento$cos2, "cos2"),
        by = c(identificador, "dimension")
      )
  }
  if (!is.null(elemento$contrib)) {
    resultado <- resultado |>
      dplyr::left_join(
        preparar(elemento$contrib, "contribucion"),
        by = c(identificador, "dimension")
      )
  }
  if (!is.null(elemento$v.test)) {
    resultado <- resultado |>
      dplyr::left_join(
        preparar(elemento$v.test, "v_test"),
        by = c(identificador, "dimension")
      )
  }
  resultado
}


resumir_famd <- function(ajuste) {
  autovalores <- tibble::tibble(
    dimension = seq_len(nrow(ajuste$eig)),
    autovalor = ajuste$eig[, 1],
    porcentaje = ajuste$eig[, 2],
    acumulado = ajuste$eig[, 3]
  )

  naturaleza <- ajuste$call$nature.var
  names(naturaleza) <- names(ajuste$call$X)
  variables <- .elemento_largo_famd(ajuste$var, "variable") |>
    dplyr::mutate(
      tipo = dplyr::recode(
        naturaleza[variable],
        quanti = "Numérica",
        quali = "Categórica"
      )
    )

  numericas <- .elemento_largo_famd(ajuste$quanti.var, "variable") |>
    dplyr::rename(correlacion = coordenada)

  mapa <- .mapa_modalidades_famd(ajuste)
  modalidades <- .elemento_largo_famd(ajuste$quali.var, "nombre_factominer") |>
    dplyr::left_join(mapa, by = "nombre_factominer")

  individuos <- .elemento_largo_famd(ajuste$ind, "individuo")

  resultado <- list(
    autovalores = autovalores,
    variables = variables,
    numericas = numericas,
    modalidades = modalidades,
    individuos = individuos
  )

  if (!is.null(ajuste$var$coord.sup)) {
    naturaleza_sup <- naturaleza[rownames(ajuste$var$coord.sup)]
    tipos_sup <- ifelse(
      naturaleza_sup == "quanti.sup",
      "Numérica suplementaria",
      "Categórica suplementaria"
    )
    coord_sup <- ajuste$var$coord.sup
    cos2_sup <- ajuste$var$cos2.sup
    elemento_sup <- list(coord = coord_sup, cos2 = cos2_sup)
    resultado$variables_suplementarias <-
      .elemento_largo_famd(elemento_sup, "variable") |>
      dplyr::mutate(tipo = tipos_sup[variable])
  }

  resultado
}


visualizar_inercia_famd <- function(ajuste) {
  tabla <- resumir_famd(ajuste)$autovalores

  ggplot2::ggplot(tabla, ggplot2::aes(dimension, porcentaje)) +
    ggplot2::geom_col(fill = "#2c7fb8", alpha = 0.85) +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "gray25") +
    ggplot2::geom_point(color = "gray25") +
    ggplot2::scale_x_continuous(breaks = tabla$dimension) +
    ggplot2::labs(
      title = "Inercia explicada por las dimensiones FAMD",
      x = "Dimensión", y = "Porcentaje de inercia"
    ) +
    ggplot2::theme_minimal()
}


visualizar_individuos_famd <- function(
  ajuste,
  ejes = c(1, 2),
  grupo = NULL,
  etiquetas = NULL
) {
  nombres_dim <- colnames(ajuste$ind$coord)[ejes]
  datos <- tibble::as_tibble(
    ajuste$ind$coord[, ejes, drop = FALSE],
    rownames = "individuo"
  )
  datos$grupo <- if (is.null(grupo)) factor("Individuos") else factor(grupo)
  datos$etiqueta <- if (is.null(etiquetas)) NA_character_ else etiquetas

  grafico <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = .data[[nombres_dim[1]]],
      y = .data[[nombres_dim[2]]],
      color = grupo
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_point(size = 2.2, alpha = 0.75) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = "Individuos en el espacio FAMD",
      x = nombres_dim[1], y = nombres_dim[2], color = NULL
    ) +
    ggplot2::theme_minimal()

  if (!is.null(etiquetas)) {
    grafico <- grafico + ggplot2::geom_text(
      ggplot2::aes(label = etiqueta),
      vjust = -0.7,
      size = 3,
      check_overlap = TRUE,
      show.legend = FALSE
    )
  }
  grafico
}


visualizar_numericas_famd <- function(
  ajuste,
  ejes = c(1, 2),
  etiquetas = TRUE
) {
  nombres_dim <- colnames(ajuste$quanti.var$coord)[ejes]
  datos <- tibble::as_tibble(
    ajuste$quanti.var$coord[, ejes, drop = FALSE],
    rownames = "variable"
  )

  angulos <- seq(0, 2 * pi, length.out = 300)
  circulo <- tibble::tibble(x = cos(angulos), y = sin(angulos))

  grafico <- ggplot2::ggplot() +
    ggplot2::geom_path(data = circulo, ggplot2::aes(x, y), color = "gray60") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray75") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray75") +
    ggplot2::geom_segment(
      data = datos,
      ggplot2::aes(
        x = 0, y = 0,
        xend = .data[[nombres_dim[1]]],
        yend = .data[[nombres_dim[2]]]
      ),
      arrow = grid::arrow(length = grid::unit(0.15, "cm")),
      color = "#2c7fb8"
    ) +
    ggplot2::coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    ggplot2::labs(
      title = "Correlaciones de las variables numéricas",
      x = nombres_dim[1], y = nombres_dim[2]
    ) +
    ggplot2::theme_minimal()

  if (etiquetas) {
    grafico <- grafico + ggplot2::geom_text(
      data = datos,
      ggplot2::aes(
        x = .data[[nombres_dim[1]]],
        y = .data[[nombres_dim[2]]],
        label = variable
      ),
      vjust = -0.6,
      check_overlap = TRUE,
      color = "#2c7fb8"
    )
  }
  grafico
}


visualizar_modalidades_famd <- function(
  ajuste,
  ejes = c(1, 2),
  etiquetas = TRUE
) {
  nombres_dim <- colnames(ajuste$quali.var$coord)[ejes]
  mapa <- .mapa_modalidades_famd(ajuste)
  datos <- tibble::as_tibble(
    ajuste$quali.var$coord[, ejes, drop = FALSE],
    rownames = "nombre_factominer"
  )
  datos$nombre_factominer <- make.unique(datos$nombre_factominer)
  datos <- dplyr::left_join(datos, mapa, by = "nombre_factominer")

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
      title = "Modalidades categóricas en el espacio FAMD",
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


visualizar_asociacion_variables_famd <- function(ajuste, ejes = c(1, 2)) {
  nombres_dim <- colnames(ajuste$var$coord)[ejes]
  naturaleza <- ajuste$call$nature.var
  names(naturaleza) <- names(ajuste$call$X)

  datos <- tibble::as_tibble(
    ajuste$var$coord[, ejes, drop = FALSE],
    rownames = "variable"
  ) |>
    tidyr::pivot_longer(-variable, names_to = "dimension", values_to = "asociacion") |>
    dplyr::mutate(
      dimension = factor(dimension, levels = nombres_dim),
      tipo = ifelse(naturaleza[variable] == "quanti", "r²", "eta²")
    )

  ggplot2::ggplot(datos, ggplot2::aes(dimension, variable, fill = asociacion)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", asociacion))) +
    ggplot2::scale_fill_viridis_c(option = "C", limits = c(0, 1)) +
    ggplot2::labs(
      title = "Relación de cada variable con las dimensiones",
      subtitle = "r² para numéricas y eta² para categóricas",
      x = NULL, y = NULL, fill = "Asociación"
    ) +
    ggplot2::theme_minimal()
}


resumir_dimension_famd <- function(ajuste, dimension = 1, n = 10) {
  naturaleza <- ajuste$call$nature.var
  names(naturaleza) <- names(ajuste$call$X)

  variables <- tibble::tibble(
    variable = rownames(ajuste$var$coord),
    asociacion = ajuste$var$coord[, dimension],
    contribucion = ajuste$var$contrib[, dimension],
    cos2 = ajuste$var$cos2[, dimension],
    tipo = ifelse(
      naturaleza[rownames(ajuste$var$coord)] == "quanti",
      "Numérica",
      "Categórica"
    )
  ) |>
    dplyr::arrange(dplyr::desc(contribucion))

  mapa <- .mapa_modalidades_famd(ajuste)
  modalidades <- tibble::tibble(
    nombre_factominer = make.unique(rownames(ajuste$quali.var$coord)),
    coordenada = ajuste$quali.var$coord[, dimension],
    contribucion = ajuste$quali.var$contrib[, dimension],
    cos2 = ajuste$quali.var$cos2[, dimension],
    v_test = ajuste$quali.var$v.test[, dimension]
  ) |>
    dplyr::left_join(mapa, by = "nombre_factominer") |>
    dplyr::arrange(dplyr::desc(contribucion))

  list(
    dimension = colnames(ajuste$var$coord)[dimension],
    variables = utils::head(variables, n),
    modalidades = utils::head(modalidades, n)
  )
}


visualizar_contribuciones_famd <- function(
  ajuste,
  dimension = 1,
  n = 10
) {
  tabla <- resumir_dimension_famd(ajuste, dimension, n)$variables

  ggplot2::ggplot(
    tabla,
    ggplot2::aes(stats::reorder(variable, contribucion), contribucion, fill = tipo)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Variables que construyen la dimensión", dimension),
      x = NULL, y = "Contribución (%)", fill = "Tipo"
    ) +
    ggplot2::theme_minimal()
}


calidad_individuos_famd <- function(ajuste, ejes = c(1, 2), n = 15) {
  calidad <- rowSums(ajuste$ind$cos2[, ejes, drop = FALSE])
  tabla <- tibble::tibble(
    individuo = rownames(ajuste$ind$cos2),
    cos2_plano = as.numeric(calidad)
  ) |>
    dplyr::arrange(cos2_plano) |>
    utils::head(n)

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(stats::reorder(individuo, cos2_plano), cos2_plano)
  ) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = "Individuos peor representados en el plano",
      x = NULL, y = "Cos² acumulado"
    ) +
    ggplot2::theme_minimal()

  list(tabla = tabla, grafico = grafico)
}
