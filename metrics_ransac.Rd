\name{metrics_ransac}
\alias{metrics_ransac}
\title{Métricas de desempenho para modelos RANSAC}
\description{Calcula RMSE, MAE e R² para o modelo ajustado via RANSAC e modelo tradicional.}
\usage{
metrics_ransac(model, data)
}
\arguments{
  \item{model}{Modelo ajustado com \code{ransac_nls} ou \code{ransac_reg}}
  \item{data}{Data frame original usado no ajuste}
}
\value{Data frame com métricas \code{RMSE}, \code{MAE}, \code{R2}.}
