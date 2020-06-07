#' discrete_plot
#'
#' This function visualizes and summarizes discrete data.
#'
#' @examples
#'
#' discrete_plot(iris, iris$Sepal.Length, "bar", "blue", xlim=c(5,7))
discrete_plot<-function(data, vector, plot, color=1, title="discrete plot",
                        xlabel=NULL, ylabel=NULL, xlim=NULL, ylim=NULL){
  library("ggplot2")
  s<-summary(vector)
  if(plot=="point"){p<-ggplot(data,aes(x = 1:length(vector), y=vector))+
    geom_point(col=color)+ggtitle(title)+ theme(plot.title = element_text(hjust=0.5))+
    xlab(xlabel)+ylab(ylabel)+coord_cartesian(xlim = xlim, ylim = ylim)}
  else if(plot=="bar"){p<-ggplot(data,aes(x=vector))+
    geom_bar(fill=color)+ggtitle(title)+ theme(plot.title = element_text(hjust=0.5))+
    xlab(xlabel)+ylab(ylabel)+coord_cartesian(xlim = xlim, ylim = ylim)}
  ret<-list(summary=s,plot=p)
  ret$str<-str(vector)
  return(ret)
}
