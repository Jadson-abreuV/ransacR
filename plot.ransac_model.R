# Conteúdo do plot.ransac_model.R (recrie conforme necessário)
#' Plot para modelos ransac_reg (lm)
#'
#' Gera um gráfico com os inliers e outliers, além da linha de predição do modelo RANSAC linear.
#'
#' @param x Modelo retornado por `ransac_reg()`.
#' @param data Data frame original com os dados.
#' @param ... Parâmetros adicionais para `ggplot2`.
#'
#' @export
plot.ransac_model <- function(x, data, ...) {
  if (missing(data)) stop("É necessário fornecer o argumento 'data'.")

  inliers <- attr(x, "inliers")
  data$tipo <- ifelse(seq_len(nrow(data)) %in% inliers, "Inlier", "Outlier")

  y_var <- all.vars(formula(x))[1]
  x_var <- all.vars(formula(x))[2]

  ggplot(data, aes_string(x = x_var, y = y_var, color = "tipo")) +
    geom_point(size = 2) +
    geom_line(aes_string(y = paste0("predict(x, newdata = data)")), linetype = "dashed") +
    labs(title = "Ajuste RANSAC (lm)", color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
}
