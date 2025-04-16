\name{plot.ransac_nls}
\alias{plot.ransac_nls}
\title{Plot para modelo não linear ajustado com RANSAC}
\description{Gera um gráfico com os inliers e outliers de um modelo nls ajustado com RANSAC.}
\usage{
\method{plot}{ransac_nls}(x, data, xvar = "D")
}
\arguments{
  \item{x}{Modelo \code{ransac_nls}}
  \item{data}{Data frame usado no ajuste}
  \item{xvar}{Variável para eixo x}
}
\value{Gráfico gerado com ggplot2.}
