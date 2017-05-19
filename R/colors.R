color_pallete<-function(num=10,type='diverging-map'){

  if (type=='diverging-map'){
    col_fun <-colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(num)
  }else if (type=='diverging-text'){
    col_fun <-colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(num)
  }else if (type=='sequential-map'){
    #printfriendly, sequentional colors from colorbrewer2.org
    col_fun <- colorRampPalette(c("#980043","#dd1c77","#df65b0","#d7b5d8","#f1eef6","#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac"),interpolate="spline")(num)
  }else if (type=='sequential-text'){
    col_fun <-colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", "cyan", "#007FFF", "blue","#00007F"))(num)
  }else {
    col_fun <-colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", "cyan", "#007FFF", "blue","#00007F"))(num)
  }

  return(col_fun)
}
