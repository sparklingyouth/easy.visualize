#' continuous_plot
#'
#' This function visualizes and summarizes continuous data.
#'
#' @examples
#'
#' continuous_plot(iris, iris$Sepal.Length, "hist", binwidth = 0.1, freq=FALSE, title="histogram")
continuous_plot<-function(data, vector, plot, color=1, binwidth=0.5, title="continuous plot",
                          xlabel=NULL, ylabel=NULL, freq=TRUE, vertical=TRUE){
  library("ggplot2")
  s<-summary(vector)
  if(plot=="hist"& freq==TRUE){p<-ggplot(data, aes(x = vector))+
    geom_histogram(fill=color, colour="white",binwidth = binwidth)+ggtitle(title)+
    theme(plot.title = element_text(hjust=0.5))+xlab(xlabel)+ylab(ylabel)}
  else if(plot=="hist"& freq==FALSE){p<-ggplot(data, aes(x = vector))+
    geom_histogram(aes(y=..density..),fill=color, colour="white",binwidth = binwidth)+ggtitle(title)+
    theme(plot.title = element_text(hjust=0.5))+xlab(xlabel)+ylab(ylabel)}
  else if(plot=="box"&vertical==TRUE) {p<-ggplot(data, aes(y = vector))+
    geom_boxplot(color=color)+ggtitle(title)+ theme(plot.title = element_text(hjust=0.5))+
    xlab(xlabel)+ylab(ylabel)}
  else if(plot=="box"&vertical==FALSE) {p<-ggplot(data, aes(x = vector))+
    geom_boxplot(color=color)+ggtitle(title)+ theme(plot.title = element_text(hjust=0.5))+
    xlab(xlabel)+ylab(ylabel)}
  ret<-list(summary=s,plot=p)
  ret$str<-str(vector)
  return(ret)
}
