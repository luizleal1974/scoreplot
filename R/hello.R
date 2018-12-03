#' @title Score Plot for Proficiency Testing
#'
#' @description This package provides score plot for proficiency testing.
#'
#' @param x
#' @param title
#' @param xlabel
#'
#' @return NULL
#'
#' @examples NULL
#'
#' @export


# --- z plot --- #
  zplot=function(x,title=NULL,xlabel=NULL){

   # Sorted data values
   x=sort(x)

   # zp function
   zp=function(x,title=NULL,xlabel=NULL,tipo){
   if(tipo==1){
    indpos=which(x>4) ; x[indpos]=4
     indneg=which(x<(-4)) ; x[indneg]=-4
      b=barplot(x,ylim=c(-5,5),main=title,xlab=xlabel) ; abline(h=c(-3,-2,0,2,3),col=c(2,4,1,4,2),lwd=c(2,2,1,2,2))
       arrows(b[indpos,],c(4.2,4.2),b[indpos,],c(4.8,4.8),lwd=7,col="dark gray")
        arrows(b[indneg,],c(-4.2,-4.2),b[indneg,],c(-4.8,-4.8),lwd=7,col="dark gray")
         }
   if(tipo==2){
    indpos=which(x>4) ; x[indpos]=4
     b=barplot(x,ylim=c(-5,5),main=title,xlab=xlabel) ; abline(h=c(-3,-2,0,2,3),col=c(2,4,1,4,2),lwd=c(2,2,1,2,2))
      arrows(b[indpos,],c(4.2,4.2),b[indpos,],c(4.8,4.8),lwd=7,col="dark gray")
       }
   if(tipo==3){
    indneg=which(x<(-4)) ; x[indneg]=-4
     b=barplot(x,ylim=c(-5,5),main=title,xlab=xlabel) ; abline(h=c(-3,-2,0,2,3),col=c(2,4,1,4,2),lwd=c(2,2,1,2,2))
      arrows(b[indneg,],c(-4.2,-4.2),b[indneg,],c(-4.8,-4.8),lwd=7,col="dark gray")
       }
   if(tipo==4){
    b=barplot(x,ylim=c(-5,5),main=title,xlab=xlabel) ; abline(h=c(-3,-2,0,2,3),col=c(2,4,1,4,2),lwd=c(2,2,1,2,2))
     }
  } # end of zp function
  
  # Plot type
  tipo=ifelse(any(x>4)==TRUE&any(x<(-4))==TRUE, 1, ifelse(any(x>4)==TRUE&any(x<(-4))!=TRUE,2,ifelse(any(x>4)!=TRUE&any(x<(-4))==TRUE,3, 4    )))
  zp(x,title,xlabel,tipo)
  }

# --- En plot --- #
  enplot=function(x,title=NULL,xlabel=NULL){

   # Sorted data values 
   x=sort(x)

   # enp function
   enp=function(x,title=NULL,xlabel=NULL,tipo){
    if(tipo==1){
     indpos=which(x>2) ; x[indpos]=2
      indneg=which(x<(-2)) ; x[indneg]=-2
       b=barplot(x,ylim=c(-2.9,2.9),main=title,xlab=xlabel) ; abline(h=c(-1,0,1),col=c(2,1,2),lwd=c(2,1,2))
        arrows(b[indpos,],c(2.2,2.2),b[indpos,],c(2.8,2.8),lwd=7,col="dark gray")
         arrows(b[indneg,],c(-2.2,-2.2),b[indneg,],c(-2.8,-2.8),lwd=7,col="dark gray")
          }
    if(tipo==2){
     indpos=which(x>2) ; x[indpos]=2
      b=barplot(x,ylim=c(-2.9,2.9),main=title,xlab=xlabel) ; abline(h=c(-1,0,1),col=c(2,1,2),lwd=c(2,1,2))
       arrows(b[indpos,],c(2.2,2.2),b[indpos,],c(2.8,2.8),lwd=7,col="dark gray")
        }
    if(tipo==3){
     indneg=which(x<(-2)) ; x[indneg]=-2
      b=barplot(x,ylim=c(-2.9,2.9),main=title,xlab=xlabel) ; abline(h=c(-1,0,1),col=c(2,1,2),lwd=c(2,1,2))
       arrows(b[indneg,],c(-2.2,-2.2),b[indneg,],c(-2.8,-2.8),lwd=7,col="dark gray")
        }
    if(tipo==4){
     b=barplot(x,ylim=c(-2.9,2.9),main=title,xlab=xlabel) ; abline(h=c(-1,0,1),col=c(2,1,2),lwd=c(2,1,2))
      }
   } # end of enp function
  
  # Plot type
  tipo=ifelse(any(x>2)==TRUE&any(x<(-2))==TRUE, 1, ifelse(any(x>2)==TRUE&any(x<(-2))!=TRUE,2,ifelse(any(x>2)!=TRUE&any(x<(-2))==TRUE,3, 4    )))
  enp(x,title,xlabel,tipo)
  }
