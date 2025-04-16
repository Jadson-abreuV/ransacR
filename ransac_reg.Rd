\name{ransac_reg}
\alias{ransac_reg}
\title{Ajuste de modelo linear via RANSAC}
\description{Ajusta um modelo linear robusto usando o algoritmo RANSAC.}
\usage{
ransac_reg(formula, data, n_min, n_iter = 100, tol = 0.2, verbose = FALSE)
}
\arguments{
  \item{formula}{Fórmula do modelo, como em \code{lm()}}
  \item{data}{Data frame contendo os dados}
  \item{n_min}{Número mínimo de observações para ajuste}
  \item{n_iter}{Número de iterações do RANSAC}
  \item{tol}{Tolerância para considerar um ponto como inlier}
  \item{verbose}{Se \code{TRUE}, mostra mensagens de progresso}
}
\value{Um modelo \code{lm} com classe adicional \code{ransac_model} e atributo \code{inliers}.}
