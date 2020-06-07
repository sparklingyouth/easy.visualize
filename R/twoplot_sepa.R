#' twoplot_sepa
#'
#' This function makes two plots and shows separated two plots at the same time and summarizes each data.
#'
#' @examples
#'
#' twoplot_sepa(iris,iris$Sepal.Length, "point", iris,iris$Petal.Length, "box")
twoplot_sepa<-function(data1, x1, plot1, data2, x2, plot2){
  library("ggplot2")
  library(gridExtra)
  s1<-summary(x1)
  s2<-summary(x2)
  d<-list(plot1=c(plot1), plot2=c(plot2), data1=data1, data2= data2, x1=x1, x2=x2, p1=NULL, p2=NULL)
  for (i in 1:2) {
    if(d[[i]]=="point"){d[[i+6]]<-ggplot(d[[i+2]]) + geom_point(aes(1:length(d[[i+4]]),d[[i+4]]))}
    else if(d[[i]]=="hist"){d[[i+6]]<-ggplot(d[[i+2]]) + geom_histogram(aes(d[[i+4]]))}
    else if(d[[i]]=="bar"){d[[i+6]]<-ggplot(d[[i+2]]) + geom_bar(aes(d[[i+4]]))}
    else if(d[[i]]=="box"){d[[i+6]]<-ggplot(d[[i+2]]) + geom_boxplot(aes(d[[i+4]]))}
  }
  p<-grid.arrange(d[["p1"]],d[["p2"]], nrow=1, ncol=2)
  res<-list(summary1=s1,summary2=s2,plot=p)
  res$str<-str(x1)
  res$str<-str(x2)
  return(res)
}
