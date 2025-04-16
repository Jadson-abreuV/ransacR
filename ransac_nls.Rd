\name{ransac_nls}
\alias{ransac_nls}
\title{Ajuste de modelo não linear via RANSAC}
\description{Ajusta um modelo não linear robusto usando o algoritmo RANSAC.}
\usage{
ransac_nls(formula, data, start, n_min, n_iter = 100, tol = 0.2, verbose = FALSE)
}
\arguments{
  \item{formula}{Fórmula do modelo, como em \code{nls()}}
  \item{data}{Data frame contendo os dados}
  \item{start}{Lista com os valores iniciais dos parâmetros}
  \item{n_min}{Número mínimo de observações para ajuste}
  \item{n_iter}{Número de iterações do RANSAC}
  \item{tol}{Tolerância para considerar um ponto como inlier}
  \item{verbose}{Se \code{TRUE}, mostra mensagens de progresso}
}
\value{Um modelo \code{nls} com classe adicional \code{ransac_nls} e atributo \code{inliers}.}
