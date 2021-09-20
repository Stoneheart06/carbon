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
aship=aship[,powerRate:=(speed/10/service_speed/0.94)^3/0.867/0.917]
aship=aship[,aPower:=powerRate*eng_total_kw]
aship=aship[sizegroup%in%c(1),powerRate:=(speed/10/service_speed/0.94)^3/0.909/0.917]
aship=aship[sizegroup%in%c(8,9),powerRate:=0.75*(speed/10/service_speed/0.94)^3/0.867/0.917]
aship=aship[powerRate<=0.07,aPower:=0]
aship=aship[speed<=10,mode:="berth"]
aship=aship[speed>10&speed<=30,mode:="anchor"]
aship=aship[speed>30&speed<=80,mode:="maneuvering"]#4th imo ghg 50
aship=aship[speed>80&powerRate<=0.65,mode:="slow"]
aship=aship[speed>80&powerRate>0.65,mode:="sea"]
#增加辅机和锅炉功率
otherPower=fread('./data/container_aux_boiler_power.csv')
aship=left_join(aship[,sizegroup:=as.integer(sizegroup)],otherPower,'sizegroup')
#查找BaseSFC(g/kwh)
aship=aship[,builtYearGroup:=cut(year_of_built,breaks=c(0,1984,2001,2050),labels=c(1,2,3),right=FALSE)]
baseSFC=fread('./data/baseSFC.csv')
#主机要根据发动机载荷进行调整
mainBSFC=as.integer(baseSFC[builtYearGroup==aship[1]$builtYearGroup&engineType=='MSD'&fuelType=='HFO',baseSFC])
aship=aship[,mainSFC:=mainBSFC*(0.455*powerRate^2-0.710*powerRate+1.28)]#4th IMO GHG p89
#辅机和锅炉不需要调整
auxBSFC=as.integer(baseSFC[builtYearGroup==aship[1]$builtYearGroup&engineType=='Auxiliary engines'&fuelType=='MDO',baseSFC])
boilerBSFC=as.integer(baseSFC[builtYearGroup==aship[1]$builtYearGroup&engineType=='Steam Turbines (and boilers)'&fuelType=='HFO',baseSFC])
aship=aship[,auxSFC:=auxBSFC][,boilerSFC:=boilerBSFC]
#fuel based emission factors
aship=aship[,mainCO2EF:=3.114][,auxCO2EF:=3.206][,boilerCO2EF:=3.206]#4th IMO GHG table21 p92,单位 g CO2/g fuel
aship=aship[,mainSO2EF:=2*0.97753*2.6][,auxSO2EF:=2*0.97753*2.6][,boilerSO2EF:=2*0.97753*2.6]#4th IMO GHG table22
aship=aship[order(time)][,timespan:=shift(time,n=-1)-time]#时间单位为秒
aship=aship[,mainCO2EM:=aPower*mainSFC*mainCO2EF*timespan/3600]#单位为克
aship=aship[,mainSO2EM:=aPower*mainSFC*mainSO2EF*timespan/3600]

aship=aship[mode%in%c('berth'),auxCO2EM:=auxBerthKW*auxSFC*auxCO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),auxCO2EM:=auxAnchorKW*auxSFC*auxCO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),auxCO2EM:=auxManKW*auxSFC*auxCO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),auxCO2EM:=auxSeaKW*auxSFC*auxCO2EF*timespan/3600]

aship=aship[mode%in%c('berth'),boilerCO2EM:=boilerBerthKW*boilerSFC*boilerCO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),boilerCO2EM:=boilerAnchorKW*boilerSFC*boilerCO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),boilerCO2EM:=boilerManKW*boilerSFC*boilerCO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),boilerCO2EM:=boilerSeaKW*boilerSFC*boilerCO2EF*timespan/3600]


#SO2排放
aship=aship[mode%in%c('berth'),auxSO2EM:=auxBerthKW*auxSFC*auxSO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),auxSO2EM:=auxAnchorKW*auxSFC*auxSO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),auxSO2EM:=auxManKW*auxSFC*auxSO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),auxSO2EM:=auxSeaKW*auxSFC*auxSO2EF*timespan/3600]

aship=aship[mode%in%c('berth'),boilerSO2EM:=boilerBerthKW*boilerSFC*boilerSO2EF*timespan/3600]
aship=aship[mode%in%c('anchor'),boilerSO2EM:=boilerAnchorKW*boilerSFC*boilerSO2EF*timespan/3600]
aship=aship[mode%in%c('maneuvering'),boilerSO2EM:=boilerManKW*boilerSFC*boilerSO2EF*timespan/3600]
aship=aship[mode%in%c('slow','sea'),boilerSO2EM:=boilerSeaKW*boilerSFC*boilerSO2EF*timespan/3600]
aship[!is.na(timespan),list(mCO2=sum(mainCO2EM),auxCO2=sum(auxCO2EM),boilerCO2=sum(boilerCO2EM),mSO2=sum(mainSO2EM),auxSO2=sum(auxSO2EM),boilerSO2=sum(boilerSO2EM)),list(mode)]



