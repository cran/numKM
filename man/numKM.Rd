\name{numKM}
\alias{numKM}
\title{Create a Kaplan-Meier Plot with Numbers at Risk}
\description{
To add the table of numbers at risk below the Kaplan-Meier plot.
}
\usage{
numKM(sfit,timeby,xlab,ylab,col,g.names)
}
\arguments{
  \item{sfit}{an object of survfit()}
  \item{timeby}{integer, the interval butween every ticks at the x axis}
  \item{xlab}{character, a title for the x axis}
  \item{ylab}{character, a title for the y axis}
  \item{col}{vector of integer or character, the colours for each group}
  \item{g.names}{vector of character, the names for each group}
}
\value{
  \item{plot}{a Kaplan-Meier plot with numbers at risk}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Please feel free to contact us, if you have any advice and find any bug!
}
\examples{
require("survival")
data(colon)
fit <- survfit(Surv(time,status)~rx, data=colon)
numKM(sfit=fit,timeby=500)
}
