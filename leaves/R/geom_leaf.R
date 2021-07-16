#' Title
#' A function that draws the shape of a leaf in R using ggplot2
#'
#' @param x
#' @param xend
#' @param f
#' @param xoffset  Controls the abscissa at the bottom of the leaf
#' @param yoffset  Controls the ordinate of the bottom of the leaf
#' @param xflip
#' @param yflip
#' @param ...    Add parameters such as color
#'
#' @return     Renders an image in the shape of a leaf on the canvas
#' @export
#'
#' @examples
#' ggplot()+coord_equal(1,c(-4,2),c(-7,3))+
#' geom_leaf(0,2,f,-1.6,-4.5,1,fill="olivedrab3",
#' color="palegreen")+geom_leaf(0,2,f,-1.6,-5,-1,
#' fill="olivedrab3",color="palegreen")
geom_leaf<-function(x,xend,f,xoffset=0,yoffset=0,
                    xflip=1,yflip=1,...)
{
  .x<-seq(x,xend,length.out=100)
  .y<-f(.x)
  df<-tibble(x=c(.x,.y),y=c(.y,.x))
  df$x<-xflip*df$x+xoffset
  df$y<-yflip*df$y+yoffset
  geom_polygon(aes(x=x,y=y),data=df,...)
}
f<-function(x) x^2/2
