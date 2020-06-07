#' twoplot_same
#'
#' This function makes two plots at the same space and summarizes each data. Two vectors are same data
#'
#' @examples
#'
#' twoplot_same(iris, iris$Sepal.Length, "point", iris$Petal.Length, "point")
twoplot_same<-function(data, vector1, plot1, vector2, plot2){
  library("ggplot2")
  s1<-summary(vector1)
  s2<-summary(vector2)
  if(plot1=="point"&plot2=="point"){
    p<-ggplot(data, aes(x = 1:length(vector1), y=vector1))+
      geom_point(color="blue")+
      geom_point(aes(x = 1:length(vector2), y=vector2), color="red")}
  else if(plot1=="point"&plot2=="line"){
    p<-ggplot(data, aes(x = 1:length(vector1), y=vector1))+
      geom_point(color="blue")+
      geom_line(aes(x = 1:length(vector2), y=vector2), color="red")}
  else if(plot1=="line"&plot2=="line"){
    p<-ggplot(data, aes(x = 1:length(vector1), y=vector1))+
      geom_line(color="blue")+
      geom_line(aes(x = 1:length(vector2), y=vector2), color="red")}
  ret<-list(summary1=s1, summary2=s2, plot=p)
  ret$str<-str(vector1)
  ret$str<-str(vector2)
  return(ret)
}
