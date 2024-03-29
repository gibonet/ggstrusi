% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tufte_boxplot.R
\name{prepare_tufte_}
\alias{prepare_tufte_}
\title{Preparation of a point and two lineranges for a boxplot variant of Tufte}
\usage{
prepare_tufte_(x = ~"", ymin, lower, middle, upper, ymax, ...)
}
\arguments{
\item{x}{one-sided formula with the name of the variable to be plotted on the x-axis}

\item{ymin}{one-sided formula with the column with the minimum value of y to be plotted}

\item{lower}{one-sided formula with the column with the lower part of the "box" of the boxplot}

\item{middle}{one-sided formula with the column with the middle point of the boxplot (usually the median)}

\item{upper}{one-sided formula with the column with the upper part of the "box" of the boxplot}

\item{ymax}{one-sided formula with the column with the column with the maximum value of y to be plotted}

\item{...}{other aesthetic mappings created by \code{\link[ggplot2]{aes_}}, passed to the 
\code{\link[ggplot2]{geom_point}} element.}
}
\value{
a list with three elements: two \code{\link[ggplot2]{geom_linerange}}s and a \code{\link[ggplot2]{geom_point}}.
}
\description{
This function prepares the elements necessary to draw a variant of the boxplot
proposed by Edward Tufte.
}
\examples{
data("c02_14")
l <- prepare_tufte_(x = ~sesso, ymin = ~p10, lower = ~p25, middle = ~p50, 
                    upper = ~p75, ymax = ~p90)
tufte_boxplot(data = c02_14[1, ], l)

l
str(l)

# With respect to `prepare_tufte`, `prepare_tufte_` easily allows to express  
# `x` as a transformation of a column. Of particular interest the usage of
# `reorder`. For example, we can do a boxplot for each group, and sort the
# data by the median wage of the group:
l <- prepare_tufte_(x = ~reorder(id, p50), ymin = ~p10, lower = ~p25, middle = ~p50, 
                    upper = ~p75, ymax = ~p90, color = ~sesso)
tufte_boxplot(data = c02_14, l)
tufte_boxplot(data = c02_14, l) + ggplot2::coord_flip()

}
\seealso{
\code{\link{prepare_tufte}} which does quite the same things but, instead of one-sided formulas, uses 
character strings to set aesthetic mappings.
}
