#' Función top_n_ef
#'
#' Recoge de los libros de pesca las n categorías (var_cat) con mayores valores de la variable numérica var_num
#'
#' @param ef eflalo
#' @param var_cat nombre de la variable categórica (Especies, artes, años...)
#' @param var_num nombre de la variable numérica (Kg, valor económico...)
#' @return df con categorías y los n valores más altos
#' @export
#'
#' @examples
#' a <- top_n_ef(Datos)
#' a

top_n_ef <- function(ef,
                     var_cat = "ori_name_sci",      # variable categórica
                     var_num = "LE_KG",      # variable numérica
                     n = 10) {     # número de categorías a devolver

  # Comprobaciones básicas
  if (!var_cat %in% names(ef))
    stop("La variable categórica no existe en el data.frame")

  if (!var_num %in% names(ef))
    stop("La variable numérica no existe en el data.frame")

  if (!is.numeric(ef[[var_num]]))
    stop("La variable numérica debe ser numérica")

  # Agregación universal
  agg <- aggregate(ef[[var_num]],
                   by = list(ef[[var_cat]]),
                   FUN = sum,
                   na.rm = TRUE)

  colnames(agg) <- c("categoria", "valor")

  # Ordenar y seleccionar top n
  agg_ord <- agg[order(agg$valor, decreasing = TRUE), ]

  # Seleccionar top n
  top_n <- head(agg_ord, n)

  # Rownames 1:n
  rownames(top_n) <- seq_len(nrow(top_n))

  return(top_n)
}
