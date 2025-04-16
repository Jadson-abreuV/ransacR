\name{plot.ransac_model}
\alias{plot.ransac_model}
\title{Plot para modelo linear ajustado com RANSAC}
\description{Gera um gráfico com os inliers e outliers de um modelo linear ajustado com RANSAC.}
\usage{
\method{plot}{ransac_model}(x, data)
}
\arguments{
  \item{x}{Modelo \code{ransac_model}}
  \item{data}{Data frame usado no ajuste}
}
\value{Gráfico gerado com ggplot2.}
