# Funciones sencillas para el tema de correspondencias simples
#
# Requisitos:
# - cargar tidyverse antes de usar las funciones;
# - usar una tabla de contingencia con frecuencias no negativas;
# - conservar nombres de filas y columnas informativos;
# - no incluir filas o columnas con frecuencia total cero;
# - ajustar el analisis con FactoMineR::CA(tabla, graph = FALSE);
# - instalar FactoMineR para ajustar el modelo.
#
# Las funciones hacen pocos chequeos de forma intencionada. La construccion de
# la tabla, la eleccion de las dimensiones y la interpretacion sustantiva de las
# categorias siguen formando parte del analisis.

resumir_tabla_contingencia <- function(tabla) {
  tabla <- as.matrix(tabla)
  prueba <- suppressWarnings(stats::chisq.test(tabla))
  n <- sum(tabla)

  esperadas <- matrix(
    as.numeric(prueba$expected),
    nrow = nrow(tabla),
    dimnames = dimnames(tabla)
  )
  residuos_pearson <- matrix(
    as.numeric(prueba$residuals),
    nrow = nrow(tabla),
    dimnames = dimnames(tabla)
  )
  residuos_ajustados <- matrix(
    as.numeric(prueba$stdres),
    nrow = nrow(tabla),
    dimnames = dimnames(tabla)
  )

  masas_fila <- rowSums(tabla) / n
  masas_columna <- colSums(tabla) / n
  v_cramer <- sqrt(
    as.numeric(prueba$statistic) /
      (n * min(nrow(tabla) - 1, ncol(tabla) - 1))
  )

  list(
    frecuencias = tabla,
    proporciones = tabla / n,
    perfiles_fila = prop.table(tabla, 1),
    perfiles_columna = prop.table(tabla, 2),
    masas_fila = tibble::tibble(categoria = rownames(tabla), masa = as.numeric(masas_fila)),
    masas_columna = tibble::tibble(categoria = colnames(tabla), masa = as.numeric(masas_columna)),
    esperadas = esperadas,
    residuos = residuos_pearson,
    residuos_pearson = residuos_pearson,
    residuos_ajustados = residuos_ajustados,
    prueba_chi2 = tibble::tibble(
      chi2 = as.numeric(prueba$statistic),
      gl = as.numeric(prueba$parameter),
      p_valor = prueba$p.value,
      v_cramer = v_cramer,
      inercia = as.numeric(prueba$statistic) / n,
      esperada_minima = min(prueba$expected),
      porcentaje_esperadas_menor_5 = 100 * mean(prueba$expected < 5)
    )
  )
}

distancias_chi2 <- function(tabla, margen = c("filas", "columnas")) {
  margen <- match.arg(margen)
  tabla <- as.matrix(tabla)
  P <- tabla / sum(tabla)
  r <- rowSums(P)
  c <- colSums(P)

  if (margen == "filas") {
    perfiles <- sweep(P, 1, r, "/")
    pesos <- c
  } else {
    perfiles <- t(sweep(P, 2, c, "/"))
    pesos <- r
  }

  perfiles_ponderados <- sweep(perfiles, 2, sqrt(pesos), "/")
  stats::dist(perfiles_ponderados)
}

visualizar_distancias_chi2 <- function(tabla, margen = c("filas", "columnas")) {
  margen <- match.arg(margen)
  matriz <- as.matrix(distancias_chi2(tabla, margen))
  datos <- tibble::as_tibble(matriz, rownames = "categoria_1") |>
    tidyr::pivot_longer(-categoria_1, names_to = "categoria_2", values_to = "distancia") |>
    dplyr::mutate(
      categoria_1 = factor(categoria_1, levels = rownames(matriz)),
      categoria_2 = factor(categoria_2, levels = rev(colnames(matriz)))
    )

  ggplot2::ggplot(datos, ggplot2::aes(categoria_1, categoria_2, fill = distancia)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", distancia))) +
    ggplot2::scale_fill_viridis_c(option = "C") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste("Distancias chi-cuadrado entre", margen),
      x = NULL, y = NULL, fill = "Distancia"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

visualizar_residuos_correspondencias <- function(
  tabla,
  tipo = c("pearson", "ajustados"),
  mostrar_numeros = TRUE
) {
  tipo <- match.arg(tipo)
  resumen <- resumir_tabla_contingencia(tabla)
  residuos <- if (tipo == "pearson") {
    resumen$residuos_pearson
  } else {
    resumen$residuos_ajustados
  }
  datos <- tibble::as_tibble(residuos, rownames = "fila") |>
    tidyr::pivot_longer(-fila, names_to = "columna", values_to = "residuo")
  limite <- max(abs(datos$residuo))

  grafico <- ggplot2::ggplot(datos, ggplot2::aes(columna, fila, fill = residuo)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac",
      midpoint = 0, limits = c(-limite, limite)
    ) +
    ggplot2::labs(
      title = paste(
        "Residuos",
        if (tipo == "pearson") "de Pearson" else "ajustados"
      ),
      x = NULL, y = NULL, fill = "Residuo"
    ) +
    ggplot2::theme_minimal()

  if (mostrar_numeros) {
    grafico <- grafico +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", residuo)))
  }
  grafico
}

resumir_correspondencias <- function(ajuste) {
  eig <- as.data.frame(ajuste$eig)
  autovalores <- tibble::tibble(
    dimension = seq_len(nrow(eig)),
    autovalor = eig[[1]],
    porcentaje = eig[[2]],
    acumulado = eig[[3]]
  )

  preparar_puntos <- function(elemento, tipo) {
    coord <- tibble::as_tibble(elemento$coord, rownames = "categoria") |>
      tidyr::pivot_longer(-categoria, names_to = "dimension", values_to = "coordenada")
    contrib <- tibble::as_tibble(elemento$contrib, rownames = "categoria") |>
      tidyr::pivot_longer(-categoria, names_to = "dimension", values_to = "contribucion")
    cos2 <- tibble::as_tibble(elemento$cos2, rownames = "categoria") |>
      tidyr::pivot_longer(-categoria, names_to = "dimension", values_to = "cos2")

    coord |>
      dplyr::left_join(contrib, by = c("categoria", "dimension")) |>
      dplyr::left_join(cos2, by = c("categoria", "dimension")) |>
      dplyr::mutate(tipo = tipo)
  }

  list(
    autovalores = autovalores,
    filas = preparar_puntos(ajuste$row, "Fila"),
    columnas = preparar_puntos(ajuste$col, "Columna")
  )
}

visualizar_inercia_correspondencias <- function(ajuste) {
  tabla <- resumir_correspondencias(ajuste)$autovalores
  ggplot2::ggplot(tabla, ggplot2::aes(dimension, porcentaje)) +
    ggplot2::geom_col(fill = "#2c7fb8", alpha = 0.85) +
    ggplot2::geom_line(ggplot2::aes(group = 1), color = "gray25") +
    ggplot2::geom_point(color = "gray25") +
    ggplot2::scale_x_continuous(breaks = tabla$dimension) +
    ggplot2::labs(
      title = "Inercia explicada por dimension",
      x = "Dimension", y = "Porcentaje de inercia"
    ) +
    ggplot2::theme_minimal()
}

visualizar_correspondencias <- function(
  ajuste,
  tipo = c("ambos", "filas", "columnas"),
  ejes = c(1, 2),
  etiquetas = TRUE,
  escalado = c("simetrico", "filas", "columnas")
) {
  tipo <- match.arg(tipo)
  escalado <- match.arg(escalado)
  nombres_dim <- colnames(ajuste$row$coord)[ejes]
  valores_singulares <- sqrt(ajuste$eig[ejes, 1])

  preparar <- function(elemento, etiqueta_tipo) {
    coordenadas <- elemento$coord[, ejes, drop = FALSE]
    usar_estandar <-
      (escalado == "filas" && etiqueta_tipo == "Columnas") ||
      (escalado == "columnas" && etiqueta_tipo == "Filas")
    if (usar_estandar) {
      coordenadas <- sweep(coordenadas, 2, valores_singulares, "/")
    }

    tibble::as_tibble(coordenadas, rownames = "categoria") |>
      dplyr::mutate(tipo = etiqueta_tipo)
  }
  datos <- dplyr::bind_rows(
    if (tipo %in% c("ambos", "filas")) preparar(ajuste$row, "Filas"),
    if (tipo %in% c("ambos", "columnas")) preparar(ajuste$col, "Columnas")
  )

  grafico <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = .data[[nombres_dim[1]]], y = .data[[nombres_dim[2]]],
      color = tipo, shape = tipo, label = categoria
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste("Mapa de correspondencias:", escalado),
      x = nombres_dim[1], y = nombres_dim[2], color = NULL, shape = NULL
    ) +
    ggplot2::theme_minimal()
  if (etiquetas) {
    grafico <- grafico + ggplot2::geom_text(
      vjust = -0.7,
      check_overlap = TRUE,
      show.legend = FALSE
    )
  }
  grafico
}

contribuciones_correspondencias <- function(
  ajuste,
  tipo = c("filas", "columnas"),
  dimension = 1,
  n = NULL
) {
  tipo <- match.arg(tipo)
  elemento <- if (tipo == "filas") ajuste$row else ajuste$col
  tabla <- tibble::tibble(
    categoria = rownames(elemento$contrib),
    contribucion = elemento$contrib[, dimension]
  ) |>
    dplyr::arrange(dplyr::desc(contribucion))
  if (!is.null(n)) tabla <- utils::head(tabla, n)

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(stats::reorder(categoria, contribucion), contribucion)
  ) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_hline(
      yintercept = 100 / nrow(elemento$contrib),
      linetype = "dashed", color = "#d95f0e"
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Contribuciones de", tipo, "a la dimension", dimension),
      x = NULL, y = "Contribucion (%)"
    ) +
    ggplot2::theme_minimal()

  list(tabla = tabla, grafico = grafico)
}

calidad_correspondencias <- function(
  ajuste,
  tipo = c("filas", "columnas"),
  ejes = c(1, 2)
) {
  tipo <- match.arg(tipo)
  elemento <- if (tipo == "filas") ajuste$row else ajuste$col
  calidad <- rowSums(elemento$cos2[, ejes, drop = FALSE])
  tabla <- tibble::tibble(
    categoria = names(calidad),
    cos2_plano = as.numeric(calidad)
  ) |>
    dplyr::arrange(cos2_plano)

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(stats::reorder(categoria, cos2_plano), cos2_plano)
  ) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = paste("Calidad de representacion de", tipo),
      x = NULL, y = "Coseno cuadrado en el plano"
    ) +
    ggplot2::theme_minimal()

  list(tabla = tabla, grafico = grafico)
}
