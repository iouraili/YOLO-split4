
args = commandArgs(trailingOnly=TRUE)

an = args[1]
outdir = args[2]
dir.create(outdir)
#outdir="."
#an ="TCGA-CQ-7067-01Z-00-DX1.7ebcaec2-3ffe-49c6-bd83-f7547aea4166_8_6.txt" 

annotation = read.delim(file = an, header = F,
           sep = " ")
names(annotation) = c("class","xcent","ycent","width","height")

plot(annotation$xcent,1-annotation$ycent,xlim = c(0,1),ylim = c(0,1))

annotation_topleft = annotation[0,]
annotation_topright = annotation[0,]
annotation_bottomleft = annotation[0,]
annotation_bottomright = annotation[0,]
annotation_store = data.frame(matrix(NA,nrow = 1,ncol = 5)) 
names(annotation_store) = c("class","xcent","ycent","width","height")

for (i in 1:nrow(annotation)){
  # all observations in top left are in: x in (0,0.5) and y in (0,0.5)
  if (annotation$xcent[i]<0.5 & annotation$ycent[i]<0.5){
    annotation_store$xcent = annotation$xcent[i]*2 
    annotation_store$ycent = annotation$ycent[i]*2
    annotation_store$width = annotation$width[i]*2
    annotation_store$height = annotation$height[i]*2
    
    xadd=annotation_store$xcent + annotation_store$width/2
    if (xadd>1){
      xdiff = xadd - 1
      annotation_store$xcent=annotation_store$xcent-(xdiff/2)
      annotation_store$width=annotation_store$width-xdiff
    }
    yadd=annotation_store$ycent + annotation_store$height/2
    if (yadd>1){
      ydiff = yadd - 1
      annotation_store$ycent=annotation_store$ycent-(ydiff/2)
      annotation_store$height=annotation_store$height-ydiff
    }
    annotation_store$class= annotation$class[i]
    annotation_topleft = rbind(annotation_topleft,annotation_store)
  }
  
  # all observations in top right are in: x in (0.5,1) and y in (0,0.5)
  if (annotation$xcent[i]>=0.5 & annotation$ycent[i]<=0.5){
    print("true")
    annotation_store$xcent = (annotation$xcent[i]-0.5)*2 
    annotation_store$ycent = annotation$ycent[i]*2 
    annotation_store$width = annotation$width[i]*2
    annotation_store$height = annotation$height[i]*2
    xdiff=annotation_store$xcent - annotation_store$width/2
    if (xdiff<0){
      annotation_store$xcent=annotation_store$xcent-(xdiff/2)
      annotation_store$width=annotation_store$width+xdiff
    }
    yadd=annotation_store$ycent + annotation_store$height/2
    if (yadd>1){
      ydiff = yadd - 1
      annotation_store$ycent=annotation_store$ycent-(ydiff/2)
      annotation_store$height=annotation_store$height-ydiff
    }
    annotation_store$class= annotation$class[i]
    annotation_topright = rbind(annotation_topright,annotation_store)
  }
  
  # all observations in bottom left are in: x in (0,0.5) and y in (0.5,1)
  if (annotation$xcent[i]<0.5 & annotation$ycent[i]>0.5){
    print("true")
    annotation_store$xcent = annotation$xcent[i]*2 
    annotation_store$ycent = (annotation$ycent[i]-0.5)*2 
    annotation_store$width = annotation$width[i]*2
    annotation_store$height = annotation$height[i]*2
    xadd=annotation_store$xcent + annotation_store$width/2
    if (xadd>1){
      xdiff = xadd - 1
      annotation_store$xcent=annotation_store$xcent-(xdiff/2)
      annotation_store$width=annotation_store$width-xdiff
    }
    ydiff=annotation_store$ycent - annotation_store$height/2
    if (ydiff<0){
      annotation_store$ycent=annotation_store$ycent-(ydiff/2)
      annotation_store$height=annotation_store$height+ydiff
    }    
    annotation_store$class= annotation$class[i]
    annotation_bottomleft = rbind(annotation_bottomleft,annotation_store)
 }
  
  # all observations in bottom right are in: x in (0.5,1) and y in (0.5,1)
  if (annotation$xcent[i]>=0.5 & annotation$ycent[i]>=0.5){
    print("true")
    annotation_store$xcent = (annotation$xcent[i]-0.5)*2 
    annotation_store$ycent = (annotation$ycent[i]-0.5)*2 
    annotation_store$width = annotation$width[i]*2
    annotation_store$height = annotation$height[i]*2
    xdiff=annotation_store$xcent - annotation_store$width/2
    if (xdiff<0){
      annotation_store$xcent=annotation_store$xcent-(xdiff/2)
      annotation_store$width=annotation_store$width+xdiff
    }
    ydiff=annotation_store$ycent - annotation_store$height/2
    if (ydiff<0){
      annotation_store$ycent=annotation_store$ycent-(ydiff/2)
      annotation_store$height=annotation_store$height+ydiff
    }
    annotation_store$class= annotation$class[i]
    annotation_bottomright = rbind(annotation_bottomright,annotation_store)
 }
}

par(mfrow=c(1,1))
plot(annotation$xcent,1-annotation$ycent,xlim = c(0,1),ylim = c(0,1))
par(mfrow=c(2,2))

plot(annotation_topleft$xcent,1-annotation_topleft$ycent,xlim = c(0,1),ylim = c(0,1))
rect(xleft=(annotation_topleft$xcent - annotation_topleft$width/2),
     xright=(annotation_topleft$xcent + annotation_topleft$width/2),
     ybottom=1-(annotation_topleft$ycent - annotation_topleft$height/2),
     ytop=1-(annotation_topleft$ycent + annotation_topleft$height/2))

plot(annotation_topright$xcent,1-annotation_topright$ycent,xlim = c(0,1),ylim = c(0,1))
rect(xleft=(annotation_topright$xcent - annotation_topright$width/2),
     xright=(annotation_topright$xcent + annotation_topright$width/2),
     ybottom=1-(annotation_topright$ycent - annotation_topright$height/2),
     ytop=1-(annotation_topright$ycent + annotation_topright$height/2))

plot(annotation_bottomleft$xcent,1-annotation_bottomleft$ycent,xlim = c(0,1),ylim = c(0,1))
rect(xleft=(annotation_bottomleft$xcent - annotation_bottomleft$width/2),
     xright=(annotation_bottomleft$xcent + annotation_bottomleft$width/2),
     ybottom=1-(annotation_bottomleft$ycent - annotation_bottomleft$height/2),
     ytop=1-(annotation_bottomleft$ycent + annotation_bottomleft$height/2))

plot(annotation_bottomright$xcent,1-annotation_bottomright$ycent,xlim = c(0,1),ylim = c(0,1))
rect(xleft=(annotation_bottomright$xcent - annotation_bottomright$width/2),
     xright=(annotation_bottomright$xcent + annotation_bottomright$width/2),
     ybottom=1-(annotation_bottomright$ycent - annotation_bottomright$height/2),
     ytop=1-(annotation_bottomright$ycent + annotation_bottomright$height/2))

file_name = basename(tools::file_path_sans_ext(an))

if (nrow(annotation_topleft)>0){
  write.table(annotation_topleft,paste0(outdir,"/",file_name,"a.txt"), quote = F, col.names = F, row.names = F, sep = " ")
} 

if (nrow(annotation_topright)>0){
  write.table(annotation_topright,paste0(outdir,"/",file_name,"b.txt"), quote = F, col.names = F, row.names = F, sep = " ")
}

if (nrow(annotation_bottomleft)>0){
  write.table(annotation_bottomleft,paste0(outdir,"/",file_name,"c.txt"), quote = F, col.names = F, row.names = F, sep = " ")
}

if (nrow(annotation_bottomright)>0){
  write.table(annotation_bottomright,paste0(outdir,"/",file_name,"d.txt"), quote = F, col.names = F, row.names = F, sep = " ")
}