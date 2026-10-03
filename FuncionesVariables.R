# Funciones auxiliares para clustering de variables
#
# Requisitos de uso:
# - Las filas representan individuos y las columnas, variables.
# - Las matrices de correlaciones o asociaciones deben ser cuadradas, simétricas
#   y conservar los nombres de las variables.
# - Para asociaciones mixtas, las variables cuantitativas deben ser numéricas,
#   las ordinales deben declararse como ordered y las nominales como factor.
# - Los datos no deben contener valores ausentes, variables constantes ni
#   factores con un solo nivel observado.
# - Los vectores de pertenencia deben estar nombrados con las variables.
# - Los objetos de variables sintéticas deben proceder de
#   ClustOfVar::cutreevar().
# - Las etiquetas opcionales se proporcionan como vectores nombrados: los
#   nombres son las variables originales y los valores, las etiquetas mostradas.
# - Se necesitan los paquetes DescTools, effectsize, ggplot2 y ClustOfVar.
#
# Las funciones hacen pocos chequeos de forma intencionada. La clasificación
# de las variables, la medida de asociación y el número de grupos forman parte
# del análisis y deben decidirse antes de llamarlas.


.asociacion_variables <- function(x, y) {
  x_ordenada <- is.numeric(x) || is.ordered(x)
  y_ordenada <- is.numeric(y) || is.ordered(y)

  if (x_ordenada && y_ordenada) {
    return(abs(stats::cor(
      as.numeric(x),
      as.numeric(y),
      method = "spearman"
    )))
  }

  if (is.factor(x) && is.factor(y)) {
    return(DescTools::CramerV(x, y))
  }

  if (x_ordenada) {
    modelo <- stats::aov(as.numeric(x) ~ y)
  } else {
    modelo <- stats::aov(as.numeric(y) ~ x)
  }

  eta_cuadrado <- effectsize::eta_squared(
    modelo,
    partial = FALSE,
    ci = NULL
  )$Eta2[1]

  sqrt(eta_cuadrado)
}


calcular_matriz_asociaciones <- function(datos) {
  variables <- names(datos)
  resultado <- matrix(
    1,
    nrow = length(variables),
    ncol = length(variables),
    dimnames = list(variables, variables)
  )

  for (i in seq_along(variables)) {
    if (i < length(variables)) {
      for (j in (i + 1):length(variables)) {
        asociacion <- .asociacion_variables(datos[[i]], datos[[j]])
        resultado[i, j] <- asociacion
        resultado[j, i] <- asociacion
      }
    }
  }

  resultado
}


mapa_calor_asociaciones <- function(
  matriz,
  etiquetas = NULL,
  tipo = c("correlacion", "intensidad"),
  digitos = 2,
  titulo = NULL
) {
  tipo <- match.arg(tipo)
  variables <- colnames(matriz)

  if (!is.null(etiquetas)) {
    nombres_mostrados <- unname(etiquetas[variables])
  } else {
    nombres_mostrados <- variables
  }

  tabla <- as.data.frame(as.table(matriz), stringsAsFactors = FALSE)
  names(tabla) <- c("variable_1", "variable_2", "asociacion")
  tabla$variable_1 <- factor(
    nombres_mostrados[match(tabla$variable_1, variables)],
    levels = nombres_mostrados
  )
  tabla$variable_2 <- factor(
    nombres_mostrados[match(tabla$variable_2, variables)],
    levels = rev(nombres_mostrados)
  )

  grafico <- ggplot2::ggplot(
    tabla,
    ggplot2::aes(x = variable_1, y = variable_2, fill = asociacion)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf(paste0("%.", digitos, "f"), asociacion)
      ),
      size = 2.7
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = titulo,
      x = NULL,
      y = NULL,
      fill = if (tipo == "correlacion") "Correlación" else "Asociación"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  if (tipo == "correlacion") {
    grafico <- grafico + ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1)
    )
  } else {
    grafico <- grafico + ggplot2::scale_fill_gradient(
      low = "#FFFDE7",
      high = "#D50000",
      limits = c(0, 1)
    )
  }

  grafico
}


calcular_disimilitud_variables <- function(matriz, potencia = 1) {
  stats::as.dist(1 - abs(matriz)^potencia)
}


resumir_correlaciones <- function(
  matriz_correlaciones,
  etiquetas = NULL,
  n = 6,
  digitos = 3
) {
  variables <- colnames(matriz_correlaciones)
  tabla <- as.data.frame(
    as.table(matriz_correlaciones),
    stringsAsFactors = FALSE
  )
  names(tabla) <- c("variable_1", "variable_2", "correlacion")
  tabla$posicion_1 <- match(tabla$variable_1, variables)
  tabla$posicion_2 <- match(tabla$variable_2, variables)
  tabla <- tabla[tabla$posicion_1 < tabla$posicion_2, ]
  tabla <- tabla[, c("variable_1", "variable_2", "correlacion")]

  if (!is.null(etiquetas)) {
    tabla$variable_1 <- unname(etiquetas[tabla$variable_1])
    tabla$variable_2 <- unname(etiquetas[tabla$variable_2])
  }

  positivas <- tabla[tabla$correlacion > 0, ]
  positivas <- positivas[order(-positivas$correlacion), ]
  negativas <- tabla[tabla$correlacion < 0, ]
  negativas <- negativas[order(negativas$correlacion), ]
  pares <- tabla[order(-abs(tabla$correlacion)), ]

  positivas <- utils::head(positivas, n)
  negativas <- utils::head(negativas, n)
  positivas$correlacion <- round(positivas$correlacion, digitos)
  negativas$correlacion <- round(negativas$correlacion, digitos)
  rownames(positivas) <- NULL
  rownames(negativas) <- NULL
  rownames(pares) <- NULL

  list(
    positivas = positivas,
    negativas = negativas,
    pares = pares
  )
}


resumir_grupos_variables <- function(
  grupos,
  etiquetas = NULL,
  nombres_grupos = NULL
) {
  variables <- names(grupos)
  if (!is.null(etiquetas)) {
    variables <- unname(etiquetas[variables])
  }

  clusters <- sort(unique(as.integer(grupos)))
  filas <- lapply(clusters, function(cluster) {
    variables_cluster <- variables[as.integer(grupos) == cluster]
    fila <- data.frame(
      cluster = cluster,
      numero_variables = length(variables_cluster),
      variables = paste(variables_cluster, collapse = ", "),
      row.names = NULL
    )

    if (!is.null(nombres_grupos)) {
      fila$nombre <- unname(nombres_grupos[cluster])
      fila <- fila[, c("cluster", "nombre", "numero_variables", "variables")]
    }

    fila
  })

  do.call(rbind, filas)
}


evaluar_variables_sinteticas <- function(
  particion,
  etiquetas = NULL,
  digitos = 3
) {
  datos <- particion$rec$X

  filas <- lapply(seq_len(particion$k), function(cluster) {
    variables <- names(particion$cluster)[
      particion$cluster == cluster
    ]
    puntuacion <- particion$scores[, cluster]

    do.call(rbind, lapply(variables, function(variable) {
      x <- datos[[variable]]

      if (is.numeric(x)) {
        correlacion <- stats::cor(x, puntuacion)
        asociacion_cuadratica <- correlacion^2
      } else {
        medias <- tapply(puntuacion, x, mean)
        frecuencias <- table(x)
        media_global <- mean(puntuacion)
        suma_total <- sum((puntuacion - media_global)^2)
        suma_entre <- sum(
          frecuencias * (medias[names(frecuencias)] - media_global)^2
        )
        correlacion <- NA_real_
        asociacion_cuadratica <- suma_entre / suma_total
      }

      data.frame(
        cluster = cluster,
        variable = variable,
        asociacion_cuadratica = asociacion_cuadratica,
        correlacion = correlacion,
        row.names = NULL
      )
    }))
  })
  tabla <- do.call(rbind, filas)

  if (!is.null(etiquetas)) {
    tabla$variable <- unname(etiquetas[tabla$variable])
  }

  tabla$asociacion_cuadratica <- round(
    tabla$asociacion_cuadratica,
    digitos
  )
  tabla$correlacion <- round(tabla$correlacion, digitos)
  tabla <- tabla[order(tabla$cluster, -tabla$asociacion_cuadratica), ]
  rownames(tabla) <- NULL

  datos_grafico <- tabla
  datos_grafico$etiqueta <- paste0(
    "C", datos_grafico$cluster, " · ", datos_grafico$variable
  )
  datos_grafico$etiqueta <- stats::reorder(
    datos_grafico$etiqueta,
    datos_grafico$asociacion_cuadratica
  )

  grafico <- ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(
      x = asociacion_cuadratica,
      y = etiqueta,
      fill = factor(cluster)
    )
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", asociacion_cuadratica)),
      hjust = -0.1,
      size = 3
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1.08)) +
    ggplot2::labs(
      title = "Calidad de representación por la variable sintética",
      x = expression(R^2),
      y = NULL
    ) +
    ggplot2::theme_minimal()

  list(tabla = tabla, grafico = grafico)
}
