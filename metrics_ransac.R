# Conteúdo do metrics_ransac.R (recrie conforme necessário)
#' Métricas de desempenho para modelos RANSAC e modelo tradicional
#'
#' Calcula e compara as métricas RMSE, MAE e R² entre o modelo ajustado com RANSAC e o modelo tradicional (com todos os dados).
#'
#' @param model Modelo ajustado via `ransac_reg()` ou `ransac_nls()`.
#' @param data Data frame original usado no ajuste.
#'
#' @return Um `data.frame` com as métricas para os dois modelos.
#' @export
metrics_ransac <- function(model, data) {
  formula_model <- formula(model)
  y_name <- all.vars(formula_model)[1]
  y_obs <- data[[y_name]]

  inliers <- attr(model, "inliers")
  data_in <- data[inliers, ]

  # --- Modelo RANSAC ---
  pred_ransac <- predict(model, newdata = data_in)
  y_inliers <- data_in[[y_name]]
  rmse_r <- sqrt(mean((y_inliers - pred_ransac)^2))
  mae_r  <- mean(abs(y_inliers - pred_ransac))
  r2_r   <- 1 - sum((y_inliers - pred_ransac)^2) / sum((y_inliers - mean(y_inliers))^2)

  # --- Modelo tradicional (com outliers) ---
  if (inherits(model, "ransac_nls")) {
    start_vals <- as.list(coef(model))  # usa os coeficientes do modelo RANSAC como chute inicial
    modelo_trad <- try(nls(formula_model, data = data, start = start_vals,
                           control = nls.control(warnOnly = TRUE)), silent = TRUE)
  } else {
    modelo_trad <- lm(formula_model, data = data)
  }

  pred_trad <- predict(modelo_trad, newdata = data)
  rmse_t <- sqrt(mean((y_obs - pred_trad)^2))
  mae_t  <- mean(abs(y_obs - pred_trad))
  r2_t   <- 1 - sum((y_obs - pred_trad)^2) / sum((y_obs - mean(y_obs))^2)

  return(data.frame(
    Modelo = c("RANSAC", "Tradicional"),
    RMSE = c(rmse_r, rmse_t),
    MAE  = c(mae_r, mae_t),
    R2   = c(r2_r, r2_t)
  ))
}
