#data arrange - transaction data
library(readxl)
library(tidyverse)
t2006 <- read_xlsx("Korea_apartment_2006.xlsx")
t2007 <- read_xlsx("Korea_apartment_2007.xlsx")
t2008 <- read_xlsx("Korea_apartment_2008.xlsx")
t2009 <- read_xlsx("Korea_apartment_2009.xlsx")
t2010 <- read_xlsx("Korea_apartment_2010.xlsx")
t2011 <- read_xlsx("Korea_apartment_2011.xlsx")
t2012 <- read_xlsx("Korea_apartment_2012.xlsx")
t2013 <- read_xlsx("Korea_apartment_2013.xlsx")
t2014 <- read_xlsx("Korea_apartment_2014.xlsx")
t2015 <- read_xlsx("Korea_apartment_2015.xlsx")
t2016 <- read_xlsx("Korea_apartment_2016.xlsx")
t2017 <- read_xlsx("Korea_apartment_2017.xlsx")
t2018 <- read_xlsx("Korea_apartment_2018.xlsx")
t2019 <- read_xlsx("Korea_apartment_2019.xlsx")
data <- rbind(t2006,t2007,t2008,t2009,t2010,t2011,t2012,t2013,t2014,t2015,t2016,t2017,t2018,t2019)
colnames(data) <- c("city", "district", "district1", "district2", "name", "size", "month", "day", "price", "floor", "built","road_name")
idx <- str_detect(data$city, "강남구")
gangnam <- data[idx,]

#write_csv(gangnam,"gangnam.csv")

#test data - transaction
test <- read_xlsx("test.xlsx")
colnames(test) <- colnames(gangnam)
#test <- rbind(t2019,test)
#write_csv(test,"test.csv")


count_fac <- function(data){
  conve <- data.frame(list = data)
  conve2 <- data.frame(list = str_remove_all(conve$list, pattern = "\\s"))
  conve3 <- map(str_extract_all(conve2$list, pattern = "\\([\\w\\,]*\\)"),function(x){map(x,str_split,"\\,")})
  conve4 <- map(conve3, function(x){sum(unlist(map(x,function(y){map(y,function(z){sum(length(z))})})))})
  return(unlist(conve4))
}
count_sub <- function(data){
  a <- map(str_split(data,","),str_extract_all,"\\w{3}")
  aa <- unlist(map(map(map(a,function(x){(map(x,length))}),unlist),sum))
}

#detail data
detail1 <- read_csv("detail1.csv")
detail2 <- read_csv("detail2.csv")
detail <- left_join(detail2, detail1, by = "KAPT_CODE")[,c(2:3,9:12,14,15)]
detail$CONVENIENT_FACILITY <- count_fac(detail$CONVENIENT_FACILITY)
detail$EDUCATION_FACILITY <- count_fac(detail$EDUCATION_FACILITY)
detail$park <- detail$KAPTD_PCNT+ detail$KAPTD_PCNTU
detail$subway <- count_sub(data = detail$SUBWAY_LINE)
detail <- detail[,c(1:4,9,10)]

gangnam$addr <- str_c(gangnam$city,gangnam$district1, sep = " ")
test$addr <- str_c(test$city,test$district1, sep = " ")
addr <- data.frame(addr = unique(gangnam$addr),stringsAsFactors = F)
addr2 <- data.frame(addr = unique(test$addr),stringsAsFactors = F)

a <- str_split(detail$ADDR, pattern = " ")
idx <- map_df(map(a,function(x){!str_detect(x[4],"^\\d")}),~data.frame(a = .x))
aa <- a[!idx]
aaa <- map_df(map(aa,function(x){paste(c(x[1:3], formatC(as.numeric(str_remove_all(x[4],"\\-.*")),width = 4, flag = "0")),collapse = " ")}),~data.frame(a = .x))
detail <- detail[!idx,]
detail$addr <- aaa$a
duplist <- data.frame(addr = unique(detail$addr[duplicated(detail$addr)]),stringsAsFactors = F)
notdupdetail <- detail[!(detail$addr %in% duplist$addr),-c(1:2)]
dup <- list()
for(i in 1:nrow(duplist)){
  dup1 <- detail[detail$addr == duplist$addr[i],]
  dup1 <- c(addr = unique(dup1$addr), round(apply(dup1[,3:6],2,mean)))
  dup[[i]] <- dup1
}
dup <- map_dfr(map(dup,unlist),as.list)
detail <- rbind(notdupdetail, dup)


final <- left_join(addr,detail,by = c("addr","addr"))
data <- left_join(gangnam,final)
data <- na.omit(data)
colnames(data) <- c("city", "district", "district1", "district2", "name", "size", "month", "day", "price", "floor", 
                    "built","road_name","addr","convenie","edu","park","subway")


final2 <- left_join(addr2, detail)
test <- left_join(test,final2)
test <- na.omit(test)
colnames(test) <- c("city", "district", "district1", "district2", "name", "size", "month", "day", "price", "floor", 
                    "built","road_name","addr","convenie","edu","park","subway")


write_csv(data,"gangnam.csv")
write_csv(test,"test.csv")
