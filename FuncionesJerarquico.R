# Funciones auxiliares para clustering jerárquico
#
# Requisitos de uso:
# - Las filas de los datos representan individuos y sus identificadores son
#   únicos.
# - Los árboles se proporcionan como objetos hclust dentro de listas nombradas.
# - Los resultados de DIANA deben convertirse antes con as.hclust().
# - Para calcular la gamma de Baker, los árboles deben contener los mismos
#   individuos.
# - Los árboles comparados deben contener los mismos individuos en el mismo
#   orden. La alineación supone el mismo número de grupos y un k pequeño.
# - La silhouette debe calcularse con la distancia elegida para evaluar la
#   partición.
# - Los datos usados para resumir grupos no deben contener valores ausentes en
#   las variables seleccionadas.
# - Se necesitan los paquetes factoextra, dendextend, cluster, ggplot2 y
#   gridExtra.
#
# Las funciones hacen pocos chequeos de forma intencionada. La preparación de
# los datos, la elección de la distancia y la construcción de los árboles
# forman parte del análisis y deben realizarse antes de llamarlas.


visualizar_arbol <- function(
  arbol,
  k = NULL,
  titulo = NULL,
  mostrar_etiquetas = FALSE,
  eje_x = "Individuos",
  eje_y = "Altura",
  colores = NULL
) {
  argumentos <- list(
    x = arbol,
    show_labels = mostrar_etiquetas,
    main = titulo
  )

  if (!is.null(k)) {
    argumentos$k <- k
    argumentos$rect <- TRUE
    argumentos$rect_fill <- FALSE
    argumentos$color_labels_by_k <- mostrar_etiquetas

    if (!is.null(colores)) {
      argumentos$k_colors <- colores
      argumentos$rect_border <- colores
    }
  }

  do.call(factoextra::fviz_dend, argumentos) +
    ggplot2::labs(x = eje_x, y = eje_y)
}


comparar_arboles <- function(
  arboles,
  k = NULL,
  columnas = 2,
  mostrar_etiquetas = FALSE,
  eje_x = "Individuos",
  eje_y = "Altura"
) {
  graficos <- lapply(names(arboles), function(nombre) {
    visualizar_arbol(
      arboles[[nombre]],
      k = k,
      titulo = nombre,
      mostrar_etiquetas = mostrar_etiquetas,
      eje_x = eje_x,
      eje_y = eje_y
    )
  })

  gridExtra::grid.arrange(grobs = graficos, ncol = columnas)
}


baker_gamma <- function(arboles) {
  n_arboles <- length(arboles)
  resultado <- matrix(
    1,
    nrow = n_arboles,
    ncol = n_arboles,
    dimnames = list(names(arboles), names(arboles))
  )

  if (n_arboles > 1) {
    for (i in seq_len(n_arboles - 1)) {
      for (j in (i + 1):n_arboles) {
        gamma <- dendextend::cor_bakers_gamma(
          arboles[[i]],
          arboles[[j]]
        )
        resultado[i, j] <- gamma
        resultado[j, i] <- gamma
      }
    }
  }

  resultado
}


comparar_particiones_jerarquico <- function(
  referencia,
  candidato,
  k
) {
  grupos_referencia <- stats::cutree(referencia, k = k)
  grupos_candidato <- stats::cutree(candidato, k = k)

  referencia_numerica <- as.integer(factor(grupos_referencia))
  candidato_numerico <- as.integer(factor(grupos_candidato))
  permutar <- function(x) {
    if (length(x) == 1) {
      return(matrix(x, nrow = 1))
    }

    do.call(rbind, lapply(seq_along(x), function(i) {
      resto <- permutar(x[-i])
      cbind(x[i], resto)
    }))
  }

  permutaciones <- permutar(seq_len(k))
  coincidencias <- apply(permutaciones, 1, function(mapa) {
    sum(referencia_numerica == mapa[candidato_numerico])
  })
  mapa_optimo <- permutaciones[which.max(coincidencias), ]
  grupos_alineados <- unname(mapa_optimo[candidato_numerico])
  coinciden <- referencia_numerica == grupos_alineados

  tabla_conteos <- table(
    referencia_numerica,
    grupos_alineados
  )
  tabla <- data.frame(
    cluster_referencia = as.integer(rownames(tabla_conteos)),
    as.data.frame.matrix(tabla_conteos),
    row.names = NULL,
    check.names = FALSE
  )
  names(tabla)[-1] <- paste0(
    "cluster_candidato_alineado_",
    colnames(tabla_conteos)
  )

  resumen <- data.frame(
    individuos = length(coinciden),
    coincidencias = sum(coinciden),
    cambios = sum(!coinciden),
    proporcion_coincidente = mean(coinciden),
    row.names = NULL
  )

  list(
    tabla = tabla,
    resumen = resumen,
    grupos_referencia = grupos_referencia,
    grupos_candidato = grupos_candidato,
    grupos_alineados = grupos_alineados,
    coinciden = coinciden
  )
}


evaluar_silhouette <- function(
  arbol,
  distancias,
  k = 2:8,
  mostrar_etiquetas = FALSE
) {
  siluetas <- lapply(k, function(numero_grupos) {
    grupos <- stats::cutree(arbol, k = numero_grupos)
    cluster::silhouette(grupos, distancias)
  })
  names(siluetas) <- paste0("k_", k)

  tabla <- do.call(rbind, lapply(seq_along(k), function(i) {
    grupos <- stats::cutree(arbol, k = k[i])
    silueta <- siluetas[[i]]

    data.frame(
      k = k[i],
      silhouette_media = mean(silueta[, "sil_width"]),
      silhouette_minima = min(silueta[, "sil_width"]),
      valores_negativos = sum(silueta[, "sil_width"] < 0),
      grupos_unitarios = sum(table(grupos) == 1),
      cluster_mas_pequeno = min(table(grupos)),
      row.names = NULL
    )
  }))

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(x = k, y = silhouette_media)
  ) +
    ggplot2::geom_line(color = "#D55E00") +
    ggplot2::geom_point(color = "#D55E00", size = 2) +
    ggplot2::scale_x_continuous(breaks = k) +
    ggplot2::labs(
      title = "Silhouette media para distintos cortes",
      x = "Número de clusters",
      y = "Silhouette media"
    ) +
    ggplot2::theme_minimal()

  graficos_silueta <- lapply(seq_along(k), function(i) {
    factoextra::fviz_silhouette(
      siluetas[[i]],
      label = mostrar_etiquetas,
      print.summary = FALSE
    ) +
      ggplot2::labs(
        title = paste("Gráfico de silhouette para k =", k[i])
      )
  })
  names(graficos_silueta) <- paste0("k_", k)

  list(
    tabla = tabla,
    grafico = grafico,
    graficos_silueta = graficos_silueta
  )
}


grafico_codo_jerarquico <- function(
  arbol,
  ultimas_fusiones = 15,
  titulo = "Últimas fusiones del árbol"
) {
  numero_fusiones <- min(ultimas_fusiones, length(arbol$height))
  datos_grafico <- data.frame(
    fusion_desde_arriba = seq_len(numero_fusiones),
    altura = rev(arbol$height)[seq_len(numero_fusiones)]
  )

  ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(x = fusion_desde_arriba, y = altura)
  ) +
    ggplot2::geom_line(color = "#0072B2") +
    ggplot2::geom_point(color = "#0072B2", size = 2) +
    ggplot2::scale_x_continuous(breaks = seq_len(numero_fusiones)) +
    ggplot2::labs(
      title = titulo,
      x = "Fusión contada desde la parte superior",
      y = "Altura"
    ) +
    ggplot2::theme_minimal()
}


primeras_fusiones <- function(arboles) {
  do.call(rbind, lapply(names(arboles), function(nombre) {
    arbol <- arboles[[nombre]]
    indices <- arbol$merge[1, ]

    data.frame(
      metodo = nombre,
      individuo_1 = arbol$labels[-indices[1]],
      individuo_2 = arbol$labels[-indices[2]],
      altura = arbol$height[1],
      row.names = NULL
    )
  }))
}


resumir_particion <- function(
  datos,
  grupos,
  variables,
  digitos = 2
) {
  datos_numericos <- as.data.frame(datos[, variables, drop = FALSE])
  grupos <- factor(grupos)
  resumen <- stats::aggregate(
    datos_numericos,
    by = list(cluster = grupos),
    FUN = mean
  )
  resumen$individuos <- as.integer(table(grupos))[match(
    as.character(resumen$cluster),
    names(table(grupos))
  )]
  resumen <- resumen[, c("cluster", "individuos", variables), drop = FALSE]
  columnas_numericas <- setdiff(names(resumen), "cluster")
  resumen[columnas_numericas] <- lapply(
    resumen[columnas_numericas],
    round,
    digits = digitos
  )

  resumen
}
