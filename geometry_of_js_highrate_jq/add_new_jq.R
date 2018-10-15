library(pacman)
p_load('sf','sp','tidyverse','maptools','tmap','spatstat','httr','jsonlite','rlist','stringi')
library(Rgctc2)
setwd('F:/Administrator/Documents/GitHub/data_universal/geometry_of_js_highrate_jq')

jq_geo=read.csv('~/GitHub/data_universal/geometry_of_js_highrate_jq/jq_geo_1010.csv',stringsAsFactors =F)
jq_geo.sf=st_as_sf(jq_geo,coords=c('lng_wgs84','lat_wgs84'),crs=4326)
jq_polygon=readRDS('jq_polygon.rds')
new_jq=jq_geo.sf[242:248,]
row.names(new_jq)=1:nrow(new_jq)
#from kml
#由于kml格式由st_read读入后其坐标的维度其实包含了高度（Z）,这个过程主要是去除这个维度重新生成面对象
kml=st_read('F:/Administrator/Documents/Map/旅游数据/景区范围/部分A级景区（1009）.kml')[c(1,4,5),]
jq_kml=filter(new_jq,aoi_id=='')

  for(i in 1:nrow(kml)){
    jq_kml[i,]$geometry=st_coordinates(kml[i,])%>%magrittr::extract(,1:2) %>%
      list %>% st_polygon() %>% st_sfc(crs=4326)

  }

#from amap aoi

aoi=new_jq %>% filter(aoi_id!="")

aoi_jq_poly=list()
for (i in 1:nrow(aoi)){
  aoi_jq_poly[[i]]=paste("https://ditu.amap.com/detail/get/detail?id=",aoi[i,]$aoi_id,sep = '')%>%
    GET() %>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE)
  Sys.sleep(sample(10:30,1))
}
x=sapply(aoi_jq_poly,sparse_aoi_polygon)


sparse_aoi_polygon=function(aoi_shape){
  aoi_shape=aoi_jq_poly[[1]]
  aoi_shape=aoi_shape$data$spec$mining_shape$shape
  aoi_shape=aoi_shape%>%str_split(';') %>% '[['(1) %>% as.matrix(ncol=1) %>% apply(2,str_split,',')
  aoi_polygon2 = aoi_shape[[1]] %>% lapply(as.numeric) %>%
    list.rbind %>% gcj02_wgs84_matrix_matrix %>% list %>%
    st_polygon
  return(aoi_polygon2)}

#from csv
jq_aoi=new_jq %>% filter(aoi_id!="")
jq_csv=read.csv('new_jq_amap.csv',header=F,stringsAsFactors = F)$V2

for(i in 1:nrow(jq_aoi)){
jq_aoi[i,]$geometry=jq_csv[i]%>%str_split(';') %>% '[['(1) %>% as.matrix(ncol=1) %>% apply(2,str_split,',')%>%
                   '[['(1)%>% lapply(as.numeric) %>%
                   list.rbind %>% gcj02_wgs84_matrix_matrix %>% list %>%
                   st_polygon %>% st_sfc(crs=4326)
}
#合并边界信息不同来源的景区，形成统一格式的sf对象
new_jq=rbind(jq_kml,jq_aoi) %>% select(Name,Rate,City=city,district,keyword,geo=geometry)
#生成wgs84和火星坐标系下不同的bbox坐标矩阵，便于查询交通等信息
get_rec_matrix=function(new_jq){

  x=st_geometry(new_jq)%>%sapply(st_bbox) %>% t
  y1=wgs84_gcj02_matrix(x[,1:2])
  y2=wgs84_gcj02_matrix(x[,3:4])
  rec_matrix=cbind(x,y1,y2) %>% as.data.frame()
  names(rec_matrix)=names(jq_fanwei)[6:13]
  return(rec_matrix)
}

rec_matrix=get_rec_matrix(new_jq)

new_jq=bind_cols(new_jq,rec_matrix)

jq_polygon=rbind(jq_polygon,new_jq)
rec_char=paste0(jq$xmin_gcj02,',',jq$ymin_gcj02,';',jq$xmax_gcj02,',',jq$ymax_gcj02)
#保存
saveRDS(jq_polygon,'jq_polygon.rds')
