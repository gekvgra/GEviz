% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_ge.R
\name{table_ge}
\alias{table_ge}
\title{Table with GE style}
\usage{
table_ge(
  df,
  titel,
  beschrieb,
  quelle = "Eigene Darstellung, GE KVG.",
  digits = 2,
  relative_width = 1
)
}
\arguments{
\item{df}{A \code{data.frame} object as input.}

\item{titel}{Character string used for the caption above of the table.}

\item{beschrieb}{Character string used for the description below the table.}

\item{quelle}{Character string declarating the source of the table.}

\item{digits}{Integer indicating the number of digits.}

\item{relative_width}{Fraction indicating the fraction of the page which the table should consume.}
}
\description{
Generated with huxtable
}
\examples{
table_ge(mtcars[1:5,], titel = "Kurzer Titel", beschrieb = "Langer Beschrieb.")

}
\seealso{
\link[huxtable]{huxtable} and for quick output see [huxtable::quick_xlsx()] and use \code{set_font("Arial")} before.
}
\author{
Yannick Schwarz
}
