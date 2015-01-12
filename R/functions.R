greennez <- function (cityname, MODISvaluename){
  Contour <- NLmun[NLmun$NAME_2 == cityname,]
  MODISmask <- mask(MODISvaluename, Contour)
  mean <- mean(MODISmask@data@values, na.rm=T)
  return (mean)
}

