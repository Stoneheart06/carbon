#calculate the emissions of a single ship 
library('data.table')
library('dplyr')


hifleet_containers=fread("D:/share/hifleet/Ships15W/hifleetships_brief.csv")[,.SD[1,],mmsi][,mmsi:=as.character(mmsi)]
#提取原始数据，该数据以船舶stop的形式与相应的港口和国家进行了关联
filepaths=dir('D:/share/2020AIS/2020_year_container/',full.names = TRUE)
mmsis=data.table(dir('D:/share/2020AIS/2020_year_container/'))[,mmsi:=tstrsplit(V1,'.',fixed = TRUE)[1]]$mmsi 
mmsis=str_trim(mmsis)
shipstopfiles0=data.table(filepath=filepaths,mmsi=mmsis)
shipstopfiles=left_join(shipstopfiles0,hifleet_containers,'mmsi')#部分船舶字段数据缺失
shipstopfiles=shipstopfiles[,teu:=as.integer(teu)][,sizegroup:=cut(teu,breaks=c(0,1000,2000,3000,5000,8000,12000,14500,20000,30000),labels=seq(1,9),right=FALSE)];shipstopfiles
shipn=nrow(shipstopfiles)

aship0=fread(shipstopfiles[1]$filepath)[speed<300]
aship=left_join(aship0,shipstopfiles[,mmsi:=as.integer(mmsi)],'mmsi')
aship=aship[,date:=as.POSIXct(time,origin="1970-01-01")][,timegroup:=cut(date,breaks='5 min')]
aship=aship[,speedgroup:=cut(speed,breaks=c(0,10,30,50,400),labels=seq(1,4),right=FALSE)];aship
aship=aship[,powerRate:=(speed/10/service_speed)^3/0.867/0.917]
aship=aship[,aPwer:=powerRate*eng_total_kw]
aship=aship[sizegroup%in%c(1),powerRate:=(speed/10/service_speed)^3/0.909/0.917]
aship=aship[sizegroup%in%c(8,9),powerRate:=0.75*(speed/10/service_speed)^3/0.867/0.917]
aship=aship[powerRate<=0.07,apower:=0]
aship=aship[speed<=10,mode:="Berth"]
aship=aship[speed>10&speed<=30,mode:="Anchor"]
aship=aship[speed>30&speed<=50,mode:="Maneuvering"]
aship=aship[speed>50&powerRate<=0.65,mode:="slow"]
aship=aship[speed>50&powerRate>0.65,mode:="sea"]