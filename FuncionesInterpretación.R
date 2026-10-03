# Funciones auxiliares para interpretar clusters
#
# Requisitos de uso:
# - Las filas representan individuos y las columnas, variables.
# - La asignación de clusters debe estar guardada en una columna de los datos.
# - El argumento cluster contiene el nombre de esa columna escrito como texto.
# - Las variables usadas en resúmenes, heatmaps y boxplots deben ser numéricas.
# - Las variables de composición pueden ser factores, caracteres o numéricas.
# - Se necesitan los paquetes dplyr, tidyr y ggplot2.
#
# Las funciones comprueban solamente la existencia de las columnas y el tipo
# numérico de las variables que lo requieren. La selección de variables y la
# interpretación sustantiva de los perfiles corresponden al analista.


.comprobar_inputs_cluster <- function(datos, cluster, variables) {
  if (!cluster %in% names(datos)) {
    stop("La columna indicada en `cluster` no existe.")
  }

  variables_ausentes <- setdiff(variables, names(datos))
  if (length(variables_ausentes) > 0) {
    stop(
      "No existen en `datos` las variables: ",
      paste(variables_ausentes, collapse = ", ")
    )
  }

  numericas <- vapply(datos[variables], is.numeric, logical(1))
  if (any(!numericas)) {
    stop("Todas las variables seleccionadas deben ser numéricas.")
  }
}


.estandarizar <- function(x) {
  as.numeric(scale(x))
}


crear_heatmap_clusters <- function(
  datos,
  cluster,
  variables = NULL
) {
  if (is.null(variables)) {
    variables <- setdiff(names(datos), cluster)
  }

  .comprobar_inputs_cluster(datos, cluster, variables)

  datos_heatmap <- datos |>
    dplyr::select(dplyr::all_of(c(cluster, variables))) |>
    dplyr::rename(cluster = dplyr::all_of(cluster)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(variables), .estandarizar)
    ) |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(variables),
        function(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "media_estandarizada"
    )

  ggplot2::ggplot(
    datos_heatmap,
    ggplot2::aes(
      x = variable,
      y = factor(cluster),
      fill = media_estandarizada
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = round(media_estandarizada, 2))
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#4575B4",
      mid = "white",
      high = "#D73027",
      midpoint = 0
    ) +
    ggplot2::labs(
      title = "Perfil estandarizado por cluster",
      x = "Variable",
      y = "Cluster",
      fill = "Media z"
    ) +
    ggplot2::theme_minimal()
}


crear_boxplots_clusters <- function(
  datos,
  cluster,
  variables = NULL
) {
  if (is.null(variables)) {
    variables <- setdiff(names(datos), cluster)
  }

  .comprobar_inputs_cluster(datos, cluster, variables)

  datos_boxplots <- datos |>
    dplyr::select(dplyr::all_of(c(cluster, variables))) |>
    dplyr::rename(cluster = dplyr::all_of(cluster)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "valor"
    )

  ggplot2::ggplot(
    datos_boxplots,
    ggplot2::aes(
      x = factor(cluster),
      y = valor,
      fill = factor(cluster)
    )
  ) +
    ggplot2::geom_boxplot(alpha = 0.8, show.legend = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y") +
    ggplot2::labs(
      title = "Distribución de las variables por cluster",
      x = "Cluster",
      y = NULL
    ) +
    ggplot2::theme_minimal()
}


tabla_resumen_clusters <- function(
  datos,
  cluster,
  variables = NULL
) {
  if (is.null(variables)) {
    variables <- setdiff(names(datos), cluster)
  }

  .comprobar_inputs_cluster(datos, cluster, variables)

  datos |>
    dplyr::select(dplyr::all_of(c(cluster, variables))) |>
    dplyr::rename(cluster = dplyr::all_of(cluster)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variables),
      names_to = "variable",
      values_to = "valor"
    ) |>
    dplyr::group_by(cluster, variable) |>
    dplyr::summarise(
      n = sum(!is.na(valor)),
      media = mean(valor, na.rm = TRUE),
      mediana = stats::median(valor, na.rm = TRUE),
      minimo = min(valor, na.rm = TRUE),
      maximo = max(valor, na.rm = TRUE),
      sd = stats::sd(valor, na.rm = TRUE),
      .groups = "drop"
    )
}


tabla_composicion_clusters <- function(datos, cluster, variable) {
  if (!cluster %in% names(datos)) {
    stop("La columna indicada en `cluster` no existe.")
  }
  if (!variable %in% names(datos)) {
    stop("La variable externa indicada no existe.")
  }

  datos |>
    dplyr::transmute(
      cluster = .data[[cluster]],
      categoria = .data[[variable]]
    ) |>
    dplyr::count(cluster, categoria, name = "frecuencia") |>
    dplyr::group_by(cluster) |>
    dplyr::mutate(
      porcentaje = round(100 * frecuencia / sum(frecuencia), 1)
    ) |>
    dplyr::ungroup()
}
