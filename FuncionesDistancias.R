# Funciones auxiliares para los apuntes de distancias
#
# Requisitos de uso:
# - Las filas de los datos representan individuos y las columnas, variables.
# - Las variables usadas con dist() y scale() deben ser numéricas.
# - Los identificadores, cuando se proporcionen, deben ser únicos.
# - Para escalar, los datos no deben contener valores ausentes ni variables
#   constantes.
# - Los objetos de distancias deben conservar las etiquetas de los individuos.
# - Las variables nominales deben ser factores, caracteres o valores lógicos.
# - Para crear tablas y gráficos se necesitan los paquetes knitr y ggplot2.
#
# Las funciones hacen pocos chequeos de forma intencionada. La selección de
# variables, el tratamiento de ausentes y la elección de la distancia forman
# parte del análisis y deben realizarse antes de llamarlas.


tablas_resumen <- function(datos, variables = names(datos), digitos = 3) {
  datos <- datos[, variables, drop = FALSE]

  son_numericas <- vapply(datos, is.numeric, logical(1))
  son_nominales <- vapply(
    datos,
    function(x) is.factor(x) || is.character(x) || is.logical(x),
    logical(1)
  )

  resumen_numerico <- do.call(
    rbind,
    lapply(names(datos)[son_numericas], function(variable) {
      x <- datos[[variable]]
      data.frame(
        variable = variable,
        minimo = min(x, na.rm = TRUE),
        maximo = max(x, na.rm = TRUE),
        media = mean(x, na.rm = TRUE),
        mediana = median(x, na.rm = TRUE),
        desviacion_tipica = sd(x, na.rm = TRUE),
        row.names = NULL
      )
    })
  )

  if (!is.null(resumen_numerico)) {
    columnas_numericas <- setdiff(names(resumen_numerico), "variable")
    resumen_numerico[columnas_numericas] <- lapply(
      resumen_numerico[columnas_numericas],
      round,
      digits = digitos
    )

    cat("### Variables numéricas\n\n")
    cat(knitr::kable(resumen_numerico), sep = "\n")
    cat("\n\n")
  }

  resumen_nominal <- lapply(names(datos)[son_nominales], function(variable) {
    frecuencias <- table(datos[[variable]], useNA = "ifany")
    data.frame(
      categoria = names(frecuencias),
      frecuencia = as.integer(frecuencias),
      porcentaje = round(100 * as.integer(frecuencias) / sum(frecuencias), digitos),
      row.names = NULL
    )
  })
  names(resumen_nominal) <- names(datos)[son_nominales]

  for (variable in names(resumen_nominal)) {
    cat("### ", variable, "\n\n", sep = "")
    cat(knitr::kable(resumen_nominal[[variable]]), sep = "\n")
    cat("\n\n")
  }

  invisible(list(
    numericas = resumen_numerico,
    nominales = resumen_nominal
  ))
}


preparar_datos_distancias <- function(
  datos,
  variables,
  id = NULL,
  estandarizar = TRUE
) {
  resultado <- as.matrix(datos[, variables, drop = FALSE])

  if (!is.null(id)) {
    rownames(resultado) <- datos[[id]]
  }

  if (estandarizar) {
    resultado <- scale(resultado)
  }

  resultado
}


tabla_distancias <- function(
  distancias,
  digitos = 2,
  primeras = NULL,
  titulo = NULL
) {
  matriz <- as.matrix(distancias)

  if (!is.null(primeras)) {
    indices <- seq_len(min(primeras, nrow(matriz)))
    matriz <- matriz[indices, indices, drop = FALSE]
  }

  knitr::kable(round(matriz, digitos), caption = titulo)
}


mapa_calor_distancias <- function(
  distancias,
  titulo = NULL,
  ordenar = FALSE,
  mostrar_etiquetas = TRUE
) {
  matriz <- as.matrix(distancias)

  if (ordenar) {
    orden <- stats::hclust(stats::as.dist(matriz))$order
    matriz <- matriz[orden, orden, drop = FALSE]
  }

  niveles <- rownames(matriz)
  datos_grafico <- as.data.frame(as.table(matriz))
  names(datos_grafico) <- c("individuo_1", "individuo_2", "distancia")
  datos_grafico$individuo_1 <- factor(datos_grafico$individuo_1, levels = niveles)
  datos_grafico$individuo_2 <- factor(datos_grafico$individuo_2, levels = niveles)

  grafico <- ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(x = individuo_1, y = individuo_2, fill = distancia)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_viridis_c(option = "C") +
    ggplot2::labs(
      title = titulo,
      x = NULL,
      y = NULL,
      fill = "Distancia"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (!mostrar_etiquetas) {
    grafico <- grafico + ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  }

  grafico
}


comparar_distancias <- function(lista_distancias, individuos) {
  valores <- vapply(lista_distancias, function(distancias) {
    matriz <- as.matrix(distancias)
    matriz[individuos[1], individuos[2]]
  }, numeric(1))

  data.frame(
    distancia = names(valores),
    valor = as.numeric(valores),
    row.names = NULL
  )
}


calcular_vecinos <- function(
  distancias,
  individuo,
  k = 5,
  datos = NULL,
  id = NULL,
  incluir = NULL
) {
  matriz <- as.matrix(distancias)
  orden <- sort(matriz[individuo, ])
  orden <- orden[names(orden) != individuo]
  orden <- head(orden, k)

  resultado <- data.frame(
    vecino = names(orden),
    distancia = as.numeric(orden),
    row.names = NULL
  )

  if (!is.null(datos)) {
    indices <- match(resultado$vecino, datos[[id]])
    informacion <- datos[indices, incluir, drop = FALSE]
    resultado <- cbind(
      resultado["vecino"],
      informacion,
      resultado["distancia"]
    )
    rownames(resultado) <- NULL
  }

  resultado
}


diferencias_perfiles <- function(datos, individuos, ordenar = TRUE) {
  datos <- as.matrix(datos)
  diferencia <- datos[individuos[1], ] - datos[individuos[2], ]

  resultado <- data.frame(
    variable = colnames(datos),
    diferencia = as.numeric(diferencia),
    diferencia_absoluta = abs(as.numeric(diferencia)),
    row.names = NULL
  )

  if (ordenar) {
    resultado <- resultado[order(-resultado$diferencia_absoluta), ]
    rownames(resultado) <- NULL
  }

  resultado
}
