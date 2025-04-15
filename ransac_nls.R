# Conteúdo do ransac_nls.R (recrie conforme necessário)
#' Ajuste de modelo não linear via RANSAC
#'
#' Ajusta um modelo não linear robusto usando `nls()` com o algoritmo RANSAC.
#'
#' @param formula Fórmula do modelo não linear (ex: V ~ a * D^b * H^c).
#' @param data Data frame com os dados de entrada.
#' @param start Lista com os valores iniciais dos parâmetros (ex: list(a = 0.01, b = 2, c = 1)).
#' @param n_min Número mínimo de pontos para ajustar o modelo.
#' @param n_iter Número de iterações do algoritmo RANSAC.
#' @param tol Tolerância para considerar um ponto como inlier (erro absoluto).
#' @param verbose Se `TRUE`, imprime o progresso durante a execução.
#'
#' @return Um modelo `nls` ajustado com os inliers, com classe adicional `"ransac_nls"` e atributo `"inliers"`.
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
