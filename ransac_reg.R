# Conteúdo do ransac_reg.R (recrie conforme necessário)
#' Ajuste de modelo linear via RANSAC
#'
#' Ajusta um modelo linear robusto (do tipo `lm`) usando o algoritmo RANSAC.
#'
#' @param formula Fórmula do modelo (como em `lm`).
#' @param data Data frame contendo as variáveis do modelo.
#' @param n_min Número mínimo de pontos para ajustar o modelo (ex: 2 para reta simples).
#' @param n_iter Número de iterações do algoritmo (quanto maior, mais robusto).
#' @param tol Tolerância para considerar um ponto como inlier (em valor absoluto).
#' @param verbose Se `TRUE`, exibe mensagens de progresso.
#'
#' @return Um modelo `lm` ajustado apenas com os inliers, com classe adicional `"ransac_model"` e atributo `"inliers"`.
#' @export
ransac_reg <- function(formula, data, n_min, n_iter = 100, tol = 0.2, verbose = FALSE) {
  melhor_inliers <- c()
  melhor_modelo <- NULL

  for (i in 1:n_iter) {
    amostra_idx <- sample(1:nrow(data), n_min)
    amostra <- data[amostra_idx, ]

    modelo_try <- try(lm(formula, data = amostra), silent = TRUE)
    if (inherits(modelo_try, "try-error")) next

    pred <- predict(modelo_try, newdata = data)
    y_var <- all.vars(formula)[1]
    erro <- abs(data[[y_var]] - pred)
    inliers <- which(erro < tol)

    if (length(inliers) > length(melhor_inliers)) {
      melhor_inliers <- inliers
      melhor_modelo <- modelo_try
      if (verbose) message("Iteração ", i, ": ", length(inliers), " inliers")
    }
  }

  modelo_final <- lm(formula, data = data[melhor_inliers, ])
  class(modelo_final) <- c("ransac_model", class(modelo_final))
  attr(modelo_final, "inliers") <- melhor_inliers
  return(modelo_final)
}
