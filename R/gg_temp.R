#' Création de graphiques à partir d'un dataframe
#'
#' @param data dataframe contenant les données à visualiser
#' @param var_x colonne du dataframe à visualiser sur l'axe x
#' @param var_y colonne du dataframe à visualiser sur l'axe y
#'
#' @return Un graphique ggplot en points et ligne avec une courbe de tendance uniquement
#' lorsqu'elle est significative
#' @export
#'
#' @importFrom ggplot2 aes coord_cartesian geom_line geom_point ggplot
#' @importFrom dplyr enquo
#'
#' @examples
#' \dontrun{g1 <- gg_temp(data = pt_data, var_y = abondance)}

gg_temp <- function(data, var_x, var_y) {

  var_x <- enquo(var_x)
  var_y <- enquo(var_y)

  g <- ggplot(data, aes(!!var_x, !!var_y)) +
    geom_point() +
    geom_line() +
    coord_cartesian(ylim = c(0,NA))

  mon_test <- lm(var_x ~ var_y, data)

  if(mon_test > 0.5) {
    g +
      geom_smooth(se = FALSE, method = "lm")
  }

  else {
    g
  }

  g

}
