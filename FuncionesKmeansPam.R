# Funciones auxiliares para k-means y PAM
#
# Requisitos de uso:
# - Las filas de los datos representan individuos y las columnas, variables.
# - Los datos usados con k-means y PCA deben ser numéricos, no contener
#   valores ausentes y estar en la escala adecuada para el análisis.
# - Los objetos de k-means deben proceder de stats::kmeans() y los de PAM, de
#   cluster::pam().
# - Los datos y las particiones comparadas deben contener los mismos individuos
#   en el mismo orden.
# - La partición candidata no debe tener más grupos que la de referencia. La
#   alineación prueba todas las correspondencias, por lo que se supone un k
#   pequeño.
# - Para deshacer un escalado, la matriz debe conservar los atributos creados
#   por scale().
# - En visualizar_clusters_pca(), los datos deben ser los mismos que se usaron
#   para ajustar k-means y PAM. Las etiquetas opcionales deben incluir las
#   columnas fila, metodo y etiqueta; metodo toma los valores "k-means" o "PAM".
# - En extraer_medoides(), id puede ser el nombre de una columna o "rownames"
#   cuando los identificadores están guardados como nombres de fila.
# - Se necesitan los paquetes ggplot2, tidyr, ggrepel y mclust.
#
# Las funciones hacen pocos chequeos de forma intencionada. La selección de
# variables, el tratamiento de ausentes, el escalado, el número de grupos y la
# elección del método forman parte del análisis y deben decidirse antes.


grafico_codo_kmeans <- function(
  datos,
  k = 1:8,
  nstart = 25,
  semilla = NULL
) {
  if (!is.null(semilla)) {
    set.seed(semilla)
  }

  suma_cuadrados <- vapply(k, function(numero_grupos) {
    stats::kmeans(
      datos,
      centers = numero_grupos,
      nstart = nstart
    )$tot.withinss
  }, numeric(1))

  tabla <- data.frame(
    k = k,
    suma_cuadrados_interna = suma_cuadrados
  )

  ggplot2::ggplot(
    tabla,
    ggplot2::aes(x = k, y = suma_cuadrados_interna)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = k) +
    ggplot2::labs(
      title = "Método del codo",
      x = "Número de clusters",
      y = "Suma de cuadrados interna"
    ) +
    ggplot2::theme_minimal()
}


evaluar_semillas_kmeans <- function(
  datos,
  k,
  semillas = 1:20,
  nstart = 1,
  ajuste_referencia = NULL
) {
  resultados <- lapply(semillas, function(semilla) {
    set.seed(semilla)
    ajuste <- stats::kmeans(datos, centers = k, nstart = nstart)

    fila <- data.frame(
      semilla = semilla,
      suma_cuadrados_interna = ajuste$tot.withinss
    )

    if (!is.null(ajuste_referencia)) {
      fila$ari_con_referencia <- mclust::adjustedRandIndex(
        ajuste$cluster,
        ajuste_referencia$cluster
      )
    }

    fila
  })

  tabla <- do.call(rbind, resultados)

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(x = semilla, y = suma_cuadrados_interna)
  ) +
    ggplot2::geom_line(color = "#0072B2") +
    ggplot2::geom_point(color = "#0072B2", size = 2) +
    ggplot2::scale_x_continuous(breaks = semillas) +
    ggplot2::labs(
      title = "Dependencia de k-means respecto a la inicialización",
      x = "Semilla",
      y = "Suma de cuadrados interna"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(ajuste_referencia)) {
    grafico <- grafico +
      ggplot2::geom_hline(
        yintercept = ajuste_referencia$tot.withinss,
        linetype = "dashed",
        color = "#D55E00"
      ) +
      ggplot2::labs(
        subtitle = "La línea discontinua corresponde al ajuste de referencia"
      )
  }

  list(tabla = tabla, grafico = grafico)
}


desescalar_centroides <- function(
  ajuste_kmeans,
  datos_escalados,
  digitos = 3
) {
  centroides <- sweep(
    ajuste_kmeans$centers,
    2,
    attr(datos_escalados, "scaled:scale"),
    "*"
  )
  centroides <- sweep(
    centroides,
    2,
    attr(datos_escalados, "scaled:center"),
    "+"
  )
  centroides <- data.frame(
    cluster = rownames(centroides),
    centroides,
    row.names = NULL,
    check.names = FALSE
  )
  centroides[-1] <- lapply(centroides[-1], round, digits = digitos)

  centroides
}


visualizar_centroides <- function(
  ajuste_kmeans,
  etiquetas_variables = NULL,
  digitos = 2
) {
  centroides <- data.frame(
    cluster = rownames(ajuste_kmeans$centers),
    ajuste_kmeans$centers,
    row.names = NULL,
    check.names = FALSE
  )
  centroides <- tidyr::pivot_longer(
    centroides,
    cols = -cluster,
    names_to = "variable",
    values_to = "z"
  )

  if (!is.null(etiquetas_variables)) {
    centroides$variable <- unname(
      etiquetas_variables[centroides$variable]
    )
  }

  centroides$cluster <- factor(centroides$cluster)
  centroides$variable <- factor(
    centroides$variable,
    levels = rev(unique(centroides$variable))
  )

  ggplot2::ggplot(
    centroides,
    ggplot2::aes(x = cluster, y = variable, fill = z)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf(paste0("%.", digitos, "f"), z)),
      size = 3
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0
    ) +
    ggplot2::labs(
      title = "Perfil tipificado de los centroides",
      x = "Cluster",
      y = NULL,
      fill = "z"
    ) +
    ggplot2::theme_minimal()
}


calcular_vecinos_centroides <- function(
  ajuste_kmeans,
  datos_escalados,
  digitos = 3
) {
  resultados <- lapply(seq_len(nrow(ajuste_kmeans$centers)), function(grupo) {
    indices <- which(ajuste_kmeans$cluster == grupo)
    diferencias <- sweep(
      datos_escalados[indices, , drop = FALSE],
      2,
      ajuste_kmeans$centers[grupo, ]
    )
    distancias <- sqrt(rowSums(diferencias^2))
    indice <- indices[which.min(distancias)]

    data.frame(
      cluster = grupo,
      observacion = rownames(datos_escalados)[indice],
      distancia = round(min(distancias), digitos),
      row.names = NULL
    )
  })

  do.call(rbind, resultados)
}


extraer_medoides <- function(
  ajuste_pam,
  datos,
  variables,
  id = NULL
) {
  indices <- ajuste_pam$id.med
  resultado <- data.frame(
    cluster = ajuste_pam$clustering[indices],
    datos[indices, variables, drop = FALSE],
    row.names = NULL,
    check.names = FALSE
  )

  if (!is.null(id)) {
    identificadores <- if (id == "rownames") {
      rownames(datos)[indices]
    } else {
      datos[[id]][indices]
    }
    resultado <- data.frame(
      cluster = resultado$cluster,
      observacion = identificadores,
      resultado[, -1, drop = FALSE],
      row.names = NULL,
      check.names = FALSE
    )
  }

  resultado[order(resultado$cluster), , drop = FALSE]
}


.alinear_particiones <- function(referencia, candidata) {
  etiquetas_referencia <- sort(unique(referencia))
  etiquetas_candidata <- sort(unique(candidata))
  k_referencia <- length(etiquetas_referencia)
  k_candidata <- length(etiquetas_candidata)

  tabla <- table(
    factor(referencia, levels = etiquetas_referencia),
    factor(candidata, levels = etiquetas_candidata)
  )

  combinaciones <- expand.grid(
    rep(list(seq_len(k_referencia)), k_candidata)
  )
  permutaciones <- combinaciones[
    apply(
      combinaciones,
      1,
      function(x) length(unique(x)) == k_candidata
    ),
    ,
    drop = FALSE
  ]
  coincidencias <- apply(permutaciones, 1, function(permutacion) {
    sum(tabla[cbind(as.integer(permutacion), seq_len(k_candidata))])
  })

  mejor <- as.integer(permutaciones[which.max(coincidencias), ])
  correspondencia <- etiquetas_referencia[mejor]
  names(correspondencia) <- etiquetas_candidata

  unname(correspondencia[as.character(candidata)])
}


comparar_particiones <- function(referencia, candidata) {
  candidata_alineada <- .alinear_particiones(referencia, candidata)
  coinciden <- referencia == candidata_alineada

  tabla_conteos <- table(referencia, candidata_alineada)
  tabla <- data.frame(
    cluster_referencia = rownames(tabla_conteos),
    as.data.frame.matrix(tabla_conteos),
    row.names = NULL,
    check.names = FALSE
  )
  names(tabla)[-1] <- paste0(
    "cluster_candidato_alineado_",
    colnames(tabla_conteos)
  )
  resumen <- data.frame(
    coincidencias = sum(coinciden),
    cambios = sum(!coinciden),
    porcentaje_coincidente = round(100 * mean(coinciden), 1),
    ari = round(mclust::adjustedRandIndex(referencia, candidata), 3),
    row.names = NULL
  )

  list(
    tabla = tabla,
    resumen = resumen,
    candidata_alineada = candidata_alineada,
    coinciden = coinciden
  )
}


visualizar_clusters_pca <- function(
  datos,
  ajuste_kmeans,
  ajuste_pam,
  etiquetas = NULL
) {
  datos <- as.matrix(datos)
  ajuste_pca <- stats::prcomp(datos, center = FALSE, scale. = FALSE)
  proporcion <- summary(ajuste_pca)$importance["Proportion of Variance", ]

  coordenadas <- data.frame(
    fila = seq_len(nrow(datos)),
    CP1 = ajuste_pca$x[, 1],
    CP2 = ajuste_pca$x[, 2]
  )
  pam_alineado <- .alinear_particiones(
    ajuste_kmeans$cluster,
    ajuste_pam$clustering
  )
  puntos <- rbind(
    transform(
      coordenadas,
      metodo = "k-means",
      cluster = factor(ajuste_kmeans$cluster)
    ),
    transform(
      coordenadas,
      metodo = "PAM",
      cluster = factor(pam_alineado)
    )
  )

  centroides <- ajuste_kmeans$centers %*%
    ajuste_pca$rotation[, 1:2, drop = FALSE]
  centroides <- data.frame(
    CP1 = centroides[, 1],
    CP2 = centroides[, 2],
    metodo = "k-means",
    tipo = "Centroide"
  )
  medoides <- data.frame(
    coordenadas[ajuste_pam$id.med, c("CP1", "CP2")],
    metodo = "PAM",
    tipo = "Medoide"
  )
  representantes <- rbind(centroides, medoides)

  grafico <- ggplot2::ggplot(
    puntos,
    ggplot2::aes(x = CP1, y = CP2, color = cluster)
  ) +
    ggplot2::stat_ellipse(
      ggplot2::aes(group = cluster),
      type = "norm",
      level = 0.80,
      linewidth = 0.6,
      alpha = 0.7
    ) +
    ggplot2::geom_point(alpha = 0.65, size = 1.8) +
    ggplot2::geom_point(
      data = representantes,
      ggplot2::aes(x = CP1, y = CP2, shape = tipo),
      inherit.aes = FALSE,
      color = "black",
      size = 3.5,
      stroke = 1.2
    ) +
    ggplot2::facet_wrap(~metodo) +
    ggplot2::scale_shape_manual(values = c(Centroide = 4, Medoide = 8)) +
    ggplot2::labs(
      title = "Proyección PCA de los individuos y sus clusters",
      subtitle = paste(
        "Las elipses contienen aproximadamente el 80% de cada grupo",
        "bajo una aproximación normal"
      ),
      x = paste0("CP1 (", scales::percent(proporcion[1], accuracy = 0.1), ")"),
      y = paste0("CP2 (", scales::percent(proporcion[2], accuracy = 0.1), ")"),
      color = "Cluster",
      shape = "Representante"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(etiquetas)) {
    etiquetas_pca <- merge(
      etiquetas,
      coordenadas,
      by = "fila",
      all.x = TRUE,
      sort = FALSE
    )
    etiquetas_pca$cluster <- ifelse(
      etiquetas_pca$metodo == "PAM",
      pam_alineado[etiquetas_pca$fila],
      ajuste_kmeans$cluster[etiquetas_pca$fila]
    )
    etiquetas_pca$cluster <- factor(
      etiquetas_pca$cluster,
      levels = levels(puntos$cluster)
    )

    grafico <- grafico +
      ggrepel::geom_text_repel(
        data = etiquetas_pca,
        ggplot2::aes(
          x = CP1,
          y = CP2,
          label = etiqueta,
          color = cluster
        ),
        inherit.aes = FALSE,
        size = 2.7,
        seed = 2026,
        max.overlaps = Inf,
        box.padding = 0.25,
        point.padding = 0.15,
        min.segment.length = 0
      )
  }

  grafico
}
