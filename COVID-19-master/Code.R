#S&I
update.packages("tools")
install.packages("ggplot2", lib="C://Program Files//R//R-4.0.4//library")
update.packages("ggplot2")
update.packages("data.table")
library(data.table)
#Move file
setwd("C://Users//LENOVO//Desktop//COVID-19-master//csse_covid_19_data//csse_covid_19_daily_reports_us")
#Read data
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill=TRUE)
names(data)
View(data)
#Tạo list & data frame
#Sỗ lượng nhiễm và chết
#Số người đã được chữa khỏi
#Tỉ lệ tử vong
# Tỉ lệ bang Michigan và Nevada
mcg <- data[Province_State=="Michigan"]
nvd <- data[Province_State=="Nevada"]
mcg$Case_Fatality_Ratio <- format(round(mcg$Case_Fatality_Ratio, 2), nsmall = 2)
nvd$Case_Fatality_Ratio <- format(round(nvd$Case_Fatality_Ratio, 2), nsmall = 2)
library("ggplot2")
#----------------------------------------------------------------
# Đọc dữ liệu từ ngày 01/01/2021
df1 <- read.table("01-01-2021.csv", header = TRUE, sep = ",")
names(df1)
# Chuyển đổi thập phân
df1$Case_Fatality_Ratio <- format(round(df1$Case_Fatality_Ratio, 2), nsmall = 2)
#----------------------------------------------------------------
# đọc giữ liệu về covid từ 05/12/2020
df <- read.table("05-12-2020.csv", header = TRUE, sep = ",")
names(df)
# chuyển đổi thập phân
df$Mortality_Rate <- format(round(df$Mortality_Rate, 2), nsmall = 2)
#----------------------------------------------------------------
# Đồ thị 1
ggplot(nvd)+
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  labs(title="Biểu đồ thể hiện tử vong của bang Nevada từ 04/2020 đến 07/2021", x="Last Update", y="Case Fatality Ratio")
#----------------------------------------------------------------
# đồ thị 2
ggplot(mcg, aes(y=Case_Fatality_Ratio, x=Last_Update, fill= Case_Fatality_Ratio)) + 
  geom_point(aes(color=Case_Fatality_Ratio)) + 
  labs(title="Biểu đồ thể hiện tử vong của bang Michigan từ 04/2020 đến 07/2021", x="Last Update", y="Case Fatality Ratio")
#----------------------------------------------------------------
# đồ thị 3
ggplot(df1[5:15,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện số lượng hồi phục trong ngày 2021-01-01") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# đồ thị 4
ggplot(mcg[1:11,], aes(x='', y=Case_Fatality_Ratio, fill=Last_Update)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong của bang Michigan từ 04/2020 đến 07/2021") +
  geom_text(aes(label = paste0(Case_Fatality_Ratio)), position = position_stack(vjust=0.5))
#----------------------------------------------------------------
#Đồ thị 5
ggplot(df1[15:30,], aes(x=Case_Fatality_Ratio, y=Province_State, color=Province_State)) + 
  geom_point() + labs(title="biểu đồ thể hiện tỉ lệ tử vong của COvid tại Mỹ ngày 01-01-2021", x="Case Fatality Ratio", y="Province")
#----------------------------------------------------------------
#Đồ thị 6
ggplot(df1[20: 26,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Biểu đồ xác nhân số ca nhiễm Covid trong ngày 01-01-2021") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
#Đồ thị 7
ggplot(df1, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="Biểu đồ thể hiện số ca hồi phục được phát hiện trong ngày 01-01-2021",x = "Confirmed", y="Province")
#----------------------------------------------------------------
# Đồ thị 8
ggplot(nvd, aes(x=Confirmed, y=Last_Update, fill = Confirmed)) + 
  geom_point(aes(colour = Confirmed)) +
  labs(title="Biểu đồ xác nhận số ca nhiễm tại Nevada từ 04/2020 đến 07/2021", x = "Confirmed", y="Last Update")
#----------------------------------------------------------------
# Đồ thị 9
ggplot(nvd) + 
  geom_line(aes(y=Case_Fatality_Ratio, x=Last_Update, color=Case_Fatality_Ratio)) +
  theme_gray() +
  labs(title="Biểu đồ tỉ lệ tử vong tại bang Nevada từ 04/2020 đến 07/2021", x="Last Update", y="Case Fatality Ratio")
#----------------------------------------------------------------
# Đồ thị 10
ggplot(df1, aes(x=Confirmed, y=Province_State, fill= Confirmed)) + 
  geom_point(aes(color=Confirmed)) +
  labs(title="Biểu đồ xác nhận số ca nhiễm Covid 19 trong ngày 01-01-2021",x = "Confirmed", y="Province")
#----------------------------------------------------------------
# Đồ thị 11
ggplot(df1[1:11,], aes(x=Deaths, y=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill = Province_State)) + 
  labs(title="Biểu đồ xác nhận số lượng tử vong do Covid 19 trong ngày 01-01-2021",x = "Deaths", y="Province")
#----------------------------------------------------------------
# Đồ thị 12
ggplot(df1[20:29,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="Biểu đồ thể hiện số ca tử vong tại mỗi bang của Mỹ trong ngày 01-01-2021")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 13
ggplot(df[1:10,], aes(x='', y=Recovered, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện số ca hồi phục xác nhân tại 1 số bang của Mỹ trong ngày 05-12-2020") +
  geom_text(aes(label = paste0(Recovered)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 14
ggplot(df[12:20,], aes(x='', y=Mortality_Rate, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() + coord_polar("y", start=0) + 
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong tại 1 số bang tại Mỹ trong ngày 05-12-2020")  +
  geom_text(aes(label = paste0(Mortality_Rate)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 15
ggplot(df[10:25,], aes(x=Mortality_Rate, y=Province_State, color=Province_State)) + 
  geom_point() + 
  labs(title="Biểu đồ thể hiện tỉ lệ tử vong tại 1 số Bang/ Thành phố ở Mỹ trong ngày 05-12-2020",x="Mortality Rate", y="Province")
#----------------------------------------------------------------
# Đồ thị 16
ggplot(df[20: 26,], aes(x='', y=Confirmed, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="DĐồ thị hiểu hiện số lượng ca nhiễm tại 1 số Bang tại Mỹ trong ngày 05-12-2020") +
  geom_text(aes(label = paste0(Confirmed)), position = position_stack(vjust=0.5)) 
#----------------------------------------------------------------
# Đồ thị 17
ggplot(df, aes(x=Recovered, y=Province_State, fill= Recovered)) + 
  geom_point(aes(color=Recovered)) + 
  labs(title="Đồ thị biểu hiện số ca hồi phục sau khi nhiễm Covid tại 1 số Bang/Thành phố tại Mỹ trong ngày 05-12-2020",x = "Recovered", y="Province")
#----------------------------------------------------------------
# Đồ thị 18
ggplot(df, aes(x=Confirmed, y=Province_State, fill= Confirmed)) + 
  geom_point(aes(color=Confirmed)) +
  labs(title="Đồ thị thể hiện số ca nhiễm Covid đc phát hiện tại 1 số Bang trong ngày 05-12-2020",x = "Confirmed", y="Province")
#----------------------------------------------------------------
# Độ thi 19
ggplot(df, aes(x=Deaths, color=Province_State)) +
  geom_col(aes(x=Deaths, y=Province_State, fill =Province_State)) + 
  theme_grey() +
  labs(title="Đồ thị thể hiện số ca tử vong được xác nhận trong ngày 05-12-2020")
#----------------------------------------------------------------
# Đồ thị 20
ggplot(mcg, aes(x=Deaths, y=Last_Update)) + 
  geom_point(aes(colour = Deaths), colour = "orange") +
  labs(title="Đồ thị thể hiện số ca tử vong bởi Covid ở bang Michigan từ 04/2020 đến 07/2021",x = "Deaths", y="Last Update")
#----------------------------------------------------------------
# Đồ thị 21
ggplot(df[5:11,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="Đồ thị thể hiện số ca tử vong được xác nhận trong ngày 05-12-2020")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 

