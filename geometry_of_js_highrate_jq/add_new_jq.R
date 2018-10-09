library(pacman)
p_load('sf','sp','tidyverse','maptools','tmap','spatstat')
p_load("httr",'tidyverse','jsonlite','sf','rgdal','rlist','rgdal','data.table','stringi')
jq_geo=read.csv('~/GitHub/data_universal/geometry_of_js_highrate_jq/jq_geo_0930.csv',stringsAsFactors =F)

row.names(new_jq)=1:nrow(new_jq)
jq_geo.sf=st_as_sf(jq_geo,coords=c('lng_wgs84','lat_wgs84'),crs=4326)
new_jq=jq_geo.sf[241:248,][-4,]
id=na.omit(new_jq)


new_jq[6,]$geo=geo[[2]]
plot(new_jq$geo)
head(new_jq,8)
plot(geo[[2]])
new_jq
geo[[1]]
kml=st_read('F:/Administrator/Documents/Map/旅游数据/景区范围/部分A级景区（1009）.kml')[c(1,4,5),]

kml_to_sf=function(kml){

  name=kml$Name %>% as.character()
  geo=list()
  for(i in 1:nrow(kml)){
  geo[[i]]=st_coordinates(kml[i,])%>%magrittr::extract(,1:2) %>%
           list %>% st_polygon() %>% st_sfc(crs=4326)
  new.sf=st_sf(Name=name[i],geo[[i]])
  kml.sf=rbind(kml.sf,new.sf)
  }

  new.sf=st_sf(Name=name[i],geo[[i]])
  kml.sf=rbind(kml.sf,new.sf)
  names(kml.sf)=c('Name','geo')

  return(kml.sf)
}


