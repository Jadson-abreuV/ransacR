#' Ajuste de modelo não linear via RANSAC
#'
#' @param formula Fórmula do modelo, como em `nls()`
#' @param data Conjunto de dados com as variáveis
#' @param start Lista com valores iniciais dos parâmetros
#' @param n_min Número mínimo de observações para ajuste
#' @param n_iter Número de iterações do RANSAC
#' @param tol Tolerância para considerar um ponto como inlier
#' @param verbose Se TRUE, mostra progresso no console
#' @return Um objeto `nls` ajustado com inliers, com classe extra `"ransac_nls"`
#' @export
ransac_nls <- function(formula, data, start, n_min, n_iter = 100, tol = 0.2, verbose = FALSE) {
  melhor_inliers <- c()
  melhor_modelo <- NULL

  for (i in 1:n_iter) {
    amostra_idx <- sample(1:nrow(data), n_min)
    amostra <- data[amostra_idx, ]

    modelo_try <- try(nls(formula, data = amostra, start = start, control = nls.control(warnOnly = TRUE)), silent = TRUE)
    if (inherits(modelo_try, "try-error")) next

    pred <- try(predict(modelo_try, newdata = data), silent = TRUE)
    if (inherits(pred, "try-error")) next

    y_obs <- data[[all.vars(formula)[1]]]
    erro <- abs(y_obs - pred)
    inliers <- which(erro < tol)

    if (length(inliers) > length(melhor_inliers)) {
      melhor_inliers <- inliers
      melhor_modelo <- modelo_try
      if (verbose) message("Iteração ", i, ": ", length(inliers), " inliers")
    }
  }

  modelo_final <- nls(formula, data = data[melhor_inliers, ], start = start, control = nls.control(warnOnly = TRUE))
  class(modelo_final) <- c("ransac_nls", class(modelo_final))
  attr(modelo_final, "inliers") <- melhor_inliers
  return(modelo_final)
}
