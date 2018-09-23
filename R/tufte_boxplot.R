

# Funzione per una variante del boxplot alla Tufte
# 1. Una linea tra il p90 e il p75
# 2. Un punto per la mediana (p50)
# 3. Una linea tra il p25 e il p10


#' A variant of boxplot proposed by Edward Tufte.
#' 
#' @param data data frame to use for plot.
#' @param ... list of elements which will be added to the (gg)plot
#' 
#' @examples 
#' data(c02_14)
#' str(c02_14)
#' 
#' # 1. Una linea tra il p90 e il p75
#' # 2. Un punto per la mediana (p50)
#' # 3. Una linea tra il p25 e il p10
#' g1 <- ggplot2::geom_linerange(ggplot2::aes(x = "", ymin = p75, ymax = p90))
#' g2 <- ggplot2::geom_point(ggplot2::aes(x = "", y = p50))
#' g3 <- ggplot2::geom_linerange(ggplot2::aes(x = "", ymin = p10, ymax = p25))
#' 
#' # E metto g1, g2 e g3 in una lista
#' g_tufte <- list(g1, g2, g3)
#' 
#' # E faccio il grafico con un gruppo (una riga del data frame)
#' tufte_boxplot(c02_14[1, ], g_tufte)
#' 
#' 
#' # Un altro esempio, stavolta utilizzando tutti i gruppi
#' # Sull'asse delle x tutti i gruppi di salariati, ordinati secondo il salario mediano
#' # Aggiungo una colonna che 'identifica' ogni riga
#' c02_14$id2 <- as.integer(row.names(c02_14))
#' g1 <- ggplot2::geom_linerange(ggplot2::aes(x = reorder(id2, p50), ymin = p75, ymax = p90))
#' g2 <- ggplot2::geom_point(ggplot2::aes(x = reorder(id2, p50), y = p50))
#' g3 <- ggplot2::geom_linerange(ggplot2::aes(x = reorder(id2, p50), ymin = p10, ymax = p25))
#' 
#' g_tufte <- list(g1, g2, g3)
#' tufte_boxplot(c02_14, g_tufte)
#' 
#' @export
tufte_boxplot <- function(data, ...){
  args_plot <- list(...)
  p <- ggplot2::ggplot(data)
  
  p + args_plot
}


# g1 <- geom_linerange(aes(x = "", ymin = p75, ymax = p90))
# g2 <- geom_point(aes(x = "", y = p50))
# g3 <- geom_linerange(aes(x = "", ymin = p10, ymax = p25))
# 
# 
# # E metto g1, g2 e g3 in una lista
# g_tufte <- list(g1, g2, g3)

# s <- scale_y_continuous(label = punto_migliaia, name = "Franchi al mese", breaks = seq(0, 20000, by = 1000))
# 
# 
# p00 <- d00 %>%
#   tufte_boxplot(g_tufte) + facet_wrap(~sesso) + t00 + scale_color_manual(values = pal_ustat) + xlab("") + s
# p00


# Provo a scrivere una funzione che crei la lista con le 3 componenti:
# - geom_linerange, geom_point e geom_linerange
# L'obiettivo è quello di replicare una chiamata di questo tipo:
# geom_boxplot(aes(x = x, ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90), stat = "identity")
# che crea un boxplot 'classico', facendo in modo che risulti un boxplot in 
# stile 'tufte' (con un punto per la mediana e due linerange...)

# geom_boxtufte <- function(
#   mapping = ggplot2::aes_string(), ...){
#   
# }

# Cambio un attimo strada e provo qualcosa di più semplice.


#' Preparation of a point and two lineranges for a boxplot variant of Tufte
#' 
#' This function prepares the elements necessary to draw a variant of the boxplot
#' proposed by Edward Tufte. 
#' 
#' @param x character string with the name of the variable to be plotted on the x-axis
#' @param ymin name of the column with the minimum value of y to be plotted
#' @param lower name of the column with the lower part of the "box" of the boxplot
#' @param middle name of the column with the middle point of the boxplot (usually the median)
#' @param upper name of the column with the upper part of the "box" of the boxplot
#' @param ymax name of the column with the maximum value of y to be plotted
#' @param ... other aesthetic mappings created by \code{\link[ggplot2]{aes_string}}, passed to the 
#' \code{\link[ggplot2]{geom_point}} element.
#' 
#' @return a list with three elements: two \code{\link[ggplot2]{geom_linerange}}s and a \code{\link[ggplot2]{geom_point}}.
#' @seealso \code{\link{prepare_tufte_}} which does quite the same things but, instead of character arguments, uses 
#' one-sided \code{\link[stats]{formula}} to set aesthetic mappings.
#'   
#' @examples 
#' data("c02_14")
#' l <- prepare_tufte(x = "sesso", ymin = "p10", lower = "p25",
#'                               middle = "p50", upper = "p75", ymax = "p90")
#'                               
#' # Tufte boxplot of the first row of the data:
#' tufte_boxplot(data = c02_14[1, ], l)
#' 
#' # Tufte boxplot of the first three rows:
#' tufte_boxplot(data = c02_14[1:3, ], l)
#' 
#' l
#' str(l)
#' 
#' # An example where we plot boxplots for each group:
#' l <- prepare_tufte(x = "id", ymin = "p10", lower = "p25",
#'                               middle = "p50", upper = "p75", ymax = "p90")
#' tufte_boxplot(data = c02_14[1, ], l)
#' @export
prepare_tufte <- function(x = "", ymin, lower, middle, upper, ymax, ...){
  a1 <- ggplot2::aes_string(x = x, ymin = upper, ymax = ymax)
  a2 <- ggplot2::aes_string(x = x, y = middle, ...)
  a3 <- ggplot2::aes_string(x = x, ymin = ymin, ymax = lower)
  
  g1 <- ggplot2::geom_linerange(a1)
  g2 <- ggplot2::geom_point(a2)
  g3 <- ggplot2::geom_linerange(a3)
  
  g_tufte <- list(g1, g2, g3)
  g_tufte
}



#' Preparation of a point and two lineranges for a boxplot variant of Tufte
#' 
#' This function prepares the elements necessary to draw a variant of the boxplot
#' proposed by Edward Tufte. 
#' 
#' @param x one-sided formula with the name of the variable to be plotted on the x-axis
#' @param ymin one-sided formula with the column with the minimum value of y to be plotted
#' @param lower one-sided formula with the column with the lower part of the "box" of the boxplot
#' @param middle one-sided formula with the column with the middle point of the boxplot (usually the median)
#' @param upper one-sided formula with the column with the upper part of the "box" of the boxplot
#' @param ymax one-sided formula with the column with the column with the maximum value of y to be plotted
#' @param ... other aesthetic mappings created by \code{\link[ggplot2]{aes_}}, passed to the 
#' \code{\link[ggplot2]{geom_point}} element.
#' 
#' @return a list with three elements: two \code{\link[ggplot2]{geom_linerange}}s and a \code{\link[ggplot2]{geom_point}}.
#' @seealso \code{\link{prepare_tufte}} which does quite the same things but, instead of one-sided formulas, uses 
#' character strings to set aesthetic mappings.
#'   
#' @examples 
#' data("c02_14")
#' l <- prepare_tufte_(x = ~sesso, ymin = ~p10, lower = ~p25, middle = ~p50, 
#'                     upper = ~p75, ymax = ~p90)
#' tufte_boxplot(data = c02_14[1, ], l)
#' 
#' l
#' str(l)
#' 
#' # With respect to `prepare_tufte`, `prepare_tufte_` easily allows to express  
#' # `x` as a transformation of a column. Of particular interest the usage of
#' # `reorder`. For example, we can do a boxplot for each group, and sort the
#' # data by the median wage of the group:
#' l <- prepare_tufte_(x = ~reorder(id, p50), ymin = ~p10, lower = ~p25, middle = ~p50, 
#'                     upper = ~p75, ymax = ~p90, color = ~sesso)
#' tufte_boxplot(data = c02_14, l)
#' tufte_boxplot(data = c02_14, l) + ggplot2::coord_flip()
#' 
#' @export
prepare_tufte_ <- function(x = ~"", ymin, lower, middle, upper, ymax, ...){
  a1 <- ggplot2::aes_(x = x, ymin = upper, ymax = ymax)
  a2 <- ggplot2::aes_(x = x, y = middle, ...)
  a3 <- ggplot2::aes_(x = x, ymin = ymin, ymax = lower)
  
  g1 <- ggplot2::geom_linerange(a1)
  g2 <- ggplot2::geom_point(a2)
  g3 <- ggplot2::geom_linerange(a3)
  
  g_tufte <- list(g1, g2, g3)
  g_tufte
}

# g1 <- geom_linerange(aes(x = "", ymin = p75, ymax = p90))
# g2 <- geom_point(aes(x = "", y = p50))
# g3 <- geom_linerange(aes(x = "", ymin = p10, ymax = p25))
# 
# 
# # E metto g1, g2 e g3 in una lista
# g_tufte <- list(g1, g2, g3)


# Scrivo una funzione che combina tufte_boxplot, prepare_tufte_ e aggiunge qualche 
# elemento che può servire in alcuni casi 'tipici' (tipo il limite y a 0, un 
# theme_tufte() dal pacchetto ggthemes, ecc...)

# Prima creo un'altra versione di prepare_tufte_, che, oltre a permettere di passare
# degli aesthetic mappings con i ... (a geom_point), permetta anche di passare delle
# impostazioni (fisse) ulteriori. Per esempio: alpha = 0.5 da impostare per tutti i 
# punti.
# aes_point <- list(x = ~x, ymin = ~ymin, lower = ~lower, middle = ~middle, upper = ~upper, ymax = ~ymax)
# aes_dots <- list(color = ~sesso, size = ~N_etp)
# aes_point_all <- c(aes_point, aes_dots)
# a2 <- do.call(ggplot2::aes_, aes_point_all)


prepare_tufte2_ <- function(x = ~"", ymin, lower, middle, upper, ymax, aes_point = list(), ...){
  a1 <- ggplot2::aes_(x = x, ymin = upper, ymax = ymax)
  
  aes_point_required <- list(x = x, y = middle)
  aes_point_all <- c(aes_point_required, aes_point)
  a2 <- do.call(ggplot2::aes_, aes_point_all)

  a3 <- ggplot2::aes_(x = x, ymin = ymin, ymax = lower)
  
  g1 <- ggplot2::geom_linerange(a1)
  g2 <- ggplot2::geom_point(a2, ...)
  g3 <- ggplot2::geom_linerange(a3)
  
  g_tufte <- list(g1, g2, g3)
  g_tufte
}


