# Funciones sencillas para el tema de analisis factorial exploratorio
#
# Requisitos:
# - cargar tidyverse antes de usar las funciones;
# - usar individuos en filas y variables numericas en columnas;
# - no incluir valores perdidos ni variables constantes;
# - ajustar los modelos con psych::fa() o stats::factanal();
# - instalar psych y GPArotation para los diagnosticos y las rotaciones;
# - pedir puntuaciones al ajustar el modelo si se quieren representar individuos.
#
# Las funciones hacen pocos chequeos de forma intencionada. La eleccion de las
# variables, del numero de factores, del metodo de extraccion y de la rotacion
# sigue siendo responsabilidad de quien realiza el analisis.

.cargas_factorial <- function(ajuste) {
  cargas <- unclass(ajuste$loadings)
  matrix(
    as.numeric(cargas),
    nrow = nrow(cargas),
    dimnames = dimnames(cargas)
  )
}

.phi_factorial <- function(ajuste) {
  cargas <- .cargas_factorial(ajuste)
  if (is.null(ajuste$Phi)) diag(ncol(cargas)) else as.matrix(ajuste$Phi)
}

comprobar_adecuacion_factorial <- function(datos) {
  datos <- as.data.frame(datos)
  datos <- datos[stats::complete.cases(datos), , drop = FALSE]
  R <- stats::cor(datos)
  kmo <- psych::KMO(R)
  bartlett <- psych::cortest.bartlett(R, n = nrow(datos))

  resumen <- tibble::tibble(
    indicador = c("KMO global", "Chi-cuadrado de Bartlett", "gl", "p-valor de Bartlett"),
    valor = c(kmo$MSA, bartlett$chisq, bartlett$df, bartlett$p.value)
  )
  kmo_variables <- tibble::tibble(
    variable = names(kmo$MSAi),
    KMO = as.numeric(kmo$MSAi)
  ) |>
    dplyr::arrange(KMO)

  list(
    resumen = resumen,
    kmo_variables = kmo_variables,
    correlaciones = R
  )
}

visualizar_numero_factores <- function(
  datos,
  simulaciones = 200,
  percentil = 0.95,
  semilla = 123
) {
  datos <- as.data.frame(datos)
  datos <- datos[stats::complete.cases(datos), , drop = FALSE]
  observado <- eigen(stats::cor(datos), symmetric = TRUE)$values

  set.seed(semilla)
  aleatorios <- replicate(simulaciones, {
    permutados <- as.data.frame(lapply(datos, sample))
    eigen(stats::cor(permutados), symmetric = TRUE)$values
  })
  referencia <- apply(aleatorios, 1, stats::quantile, probs = percentil)

  tabla <- tibble::tibble(
    factor = seq_along(observado),
    observado = observado,
    referencia_aleatoria = referencia,
    supera_referencia = observado > referencia
  )
  datos_grafico <- tabla |>
    dplyr::select(factor, observado, referencia_aleatoria) |>
    tidyr::pivot_longer(-factor, names_to = "serie", values_to = "autovalor")

  grafico <- ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(factor, autovalor, color = serie, group = serie)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "gray55") +
    ggplot2::scale_x_continuous(breaks = tabla$factor) +
    ggplot2::labs(
      title = "Scree plot y referencia del analisis paralelo",
      x = "Numero de factores", y = "Autovalor", color = NULL
    ) +
    ggplot2::theme_minimal()

  list(
    tabla = tabla,
    sugerencia_paralelo = sum(tabla$supera_referencia),
    grafico = grafico
  )
}

resumir_factorial <- function(ajuste, umbral = 0.30) {
  cargas <- .cargas_factorial(ajuste)
  unicidades <- as.numeric(ajuste$uniquenesses)
  nombres <- rownames(cargas)
  factores <- colnames(cargas)

  cargas_largas <- tibble::as_tibble(cargas, rownames = "variable") |>
    tidyr::pivot_longer(-variable, names_to = "factor", values_to = "carga") |>
    dplyr::mutate(carga_absoluta = abs(carga))

  variables <- cargas_largas |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      factor_principal = factor[which.max(carga_absoluta)],
      carga_principal = carga[which.max(carga_absoluta)],
      numero_cargas_relevantes = sum(carga_absoluta >= umbral),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      comunalidad = 1 - unicidades[match(variable, nombres)],
      unicidad = unicidades[match(variable, nombres)],
      carga_cruzada = numero_cargas_relevantes > 1
    )

  list(
    cargas = cargas_largas,
    variables = variables,
    matriz_cargas = cargas,
    correlacion_factores = .phi_factorial(ajuste)
  )
}

variables_por_factor <- function(ajuste, umbral = 0.30) {
  resumir_factorial(ajuste, umbral)$cargas |>
    dplyr::filter(carga_absoluta >= umbral) |>
    dplyr::arrange(factor, dplyr::desc(carga_absoluta))
}

visualizar_varianza_factorial <- function(ajuste) {
  resumen <- resumir_factorial(ajuste)$variables |>
    dplyr::select(variable, comunalidad, unicidad) |>
    tidyr::pivot_longer(-variable, names_to = "tipo", values_to = "proporcion")

  ggplot2::ggplot(
    resumen,
    ggplot2::aes(stats::reorder(variable, proporcion * (tipo == "comunalidad")), proporcion, fill = tipo)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(comunalidad = "#2c7fb8", unicidad = "#d9d9d9"),
      labels = c(comunalidad = "Comun", unicidad = "Unica")
    ) +
    ggplot2::labs(
      title = "Descomposicion de la varianza",
      x = NULL, y = "Proporcion", fill = NULL
    ) +
    ggplot2::theme_minimal()
}

visualizar_matriz_cargas <- function(ajuste, mostrar_desde = 0) {
  datos <- resumir_factorial(ajuste)$cargas |>
    dplyr::mutate(etiqueta = dplyr::if_else(
      carga_absoluta >= mostrar_desde,
      sprintf("%.2f", carga),
      ""
    ))

  ggplot2::ggplot(datos, ggplot2::aes(factor, variable, fill = carga)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = etiqueta)) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac",
      midpoint = 0, limits = c(-1, 1)
    ) +
    ggplot2::labs(title = "Matriz de cargas factoriales", x = NULL, y = NULL, fill = "Carga") +
    ggplot2::theme_minimal()
}

visualizar_cargas_factoriales <- function(ajuste, factores = c(1, 2)) {
  cargas <- .cargas_factorial(ajuste)[, factores, drop = FALSE]
  datos <- tibble::as_tibble(cargas, rownames = "variable")
  nombres <- colnames(cargas)

  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = .data[[nombres[1]]], y = .data[[nombres[2]]], label = variable)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, y = 0, xend = .data[[nombres[1]]], yend = .data[[nombres[2]]]),
      arrow = grid::arrow(length = grid::unit(0.12, "cm")), color = "#2c7fb8"
    ) +
    ggplot2::geom_text(vjust = -0.6, check_overlap = TRUE) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Variables en el espacio factorial", x = nombres[1], y = nombres[2]) +
    ggplot2::theme_minimal()
}

comparar_rotaciones_factoriales <- function(ajustes, factores = c(1, 2)) {
  datos <- dplyr::bind_rows(lapply(names(ajustes), function(nombre) {
    cargas <- .cargas_factorial(ajustes[[nombre]])[, factores, drop = FALSE]
    tibble::as_tibble(cargas, rownames = "variable") |>
      dplyr::mutate(solucion = nombre)
  }))
  nombres <- colnames(.cargas_factorial(ajustes[[1]])[, factores, drop = FALSE])

  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = .data[[nombres[1]]], y = .data[[nombres[2]]], label = variable)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray75") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray75") +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, y = 0, xend = .data[[nombres[1]]], yend = .data[[nombres[2]]]),
      arrow = grid::arrow(length = grid::unit(0.10, "cm")), color = "#2c7fb8"
    ) +
    ggplot2::geom_text(vjust = -0.5, check_overlap = TRUE, size = 3) +
    ggplot2::facet_wrap(ggplot2::vars(solucion)) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Efecto de la rotacion sobre las cargas", x = nombres[1], y = nombres[2]) +
    ggplot2::theme_minimal()
}

comparar_correlaciones_factorial <- function(datos, ajuste) {
  observada <- stats::cor(as.data.frame(datos), use = "complete.obs")
  cargas <- .cargas_factorial(ajuste)
  comun <- cargas %*% .phi_factorial(ajuste) %*% t(cargas)
  reproducida <- comun + diag(as.numeric(ajuste$uniquenesses))
  dimnames(reproducida) <- dimnames(observada)
  residuos <- observada - reproducida

  datos_grafico <- tibble::as_tibble(residuos, rownames = "variable_1") |>
    tidyr::pivot_longer(-variable_1, names_to = "variable_2", values_to = "residuo")
  limite <- max(abs(datos_grafico$residuo))
  grafico <- ggplot2::ggplot(
    datos_grafico,
    ggplot2::aes(variable_1, variable_2, fill = residuo)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", residuo)), size = 3) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac",
      midpoint = 0, limits = c(-limite, limite)
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Residuos de correlacion", x = NULL, y = NULL, fill = "Residuo") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  list(
    observada = observada,
    reproducida = reproducida,
    residuos = residuos,
    rmsr = sqrt(mean(residuos[lower.tri(residuos)]^2)),
    grafico = grafico
  )
}

visualizar_puntuaciones_factoriales <- function(ajuste, id = NULL, factores = c(1, 2)) {
  scores <- as.matrix(ajuste$scores)[, factores, drop = FALSE]
  if (is.null(id)) id <- rownames(scores)
  if (is.null(id)) id <- seq_len(nrow(scores))
  datos <- tibble::as_tibble(scores) |>
    dplyr::mutate(id = id)
  nombres <- colnames(scores)

  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = .data[[nombres[1]]], y = .data[[nombres[2]]], label = id)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    ggplot2::geom_point(color = "#2c7fb8", alpha = 0.8) +
    ggplot2::geom_text(vjust = -0.6, check_overlap = TRUE, size = 3) +
    ggplot2::labs(title = "Puntuaciones factoriales estimadas", x = nombres[1], y = nombres[2]) +
    ggplot2::theme_minimal()
}
