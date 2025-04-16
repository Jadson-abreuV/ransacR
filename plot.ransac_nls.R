# Conteúdo do plot.ransac_nls.R (recrie conforme necessário)
#' Plot para modelos ransac_nls (nls)
#'
#' Gera um gráfico com os inliers e outliers, e a curva de predição para modelos não lineares ajustados via RANSAC.
#'
#' @param x Modelo retornado por `ransac_nls()`.
#' @param data Data frame original com os dados.
#' @param xvar Nome da variável do eixo x (ex: "D").
#' @param ... Parâmetros adicionais para `ggplot2`.
#'
#' @export
plot.ransac_nls <- function(x, data, xvar = "D", ...) {
  if (missing(data)) stop("É necessário fornecer o argumento 'data'.")

  inliers <- attr(x, "inliers")
  data$tipo <- ifelse(seq_len(nrow(data)) %in% inliers, "Inlier", "Outlier")
  data$y_pred <- predict(x, newdata = data)

  yvar <- all.vars(formula(x))[1]  # identifica o nome da variável resposta automaticamente

  ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]], color = tipo)) +
    geom_point(size = 2) +
    geom_line(aes(y = y_pred), linetype = "dashed") +
    labs(title = paste0("Ajuste RANSAC (nls): ", yvar, " ~ ", xvar),
         x = xvar, y = yvar, color = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
}
