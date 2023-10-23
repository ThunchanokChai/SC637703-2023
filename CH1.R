#12/07/2023
##CH1
###### Data maniputation #####

install.packages(c('dplyr','tidyverse'))
library('dplyr')
library('tidyverse')
X1 = matrix(c(2,3,7,4),nrow=4,ncol=1)
X1
X2 = matrix(c(42,52,48,58,4,5,4,5),nrow=4,ncol=2)
X2

# Create the data frame name df1
#df1 = data.frame(CustomerId = c(1:6), Product = c("Oven","Television","Mobile","WashingMachine","Lightings","Ipad"),Num_Product=c("10","100","220","20","5","10")) #Num_Productเป็นข้อความ ต้องเปลี่ยนเป็นตัวเลข
df1 = data.frame(CustomerId = c(1:6), Product = c("Oven","Television","Mobile","WashingMachine","Lightings","Ipad"),Num_Product=c(10,100,220,20,5,10))
df1
df2 = data.frame(CustomerId = c(2,4,6,7,8), State = c("Claifornia","Newyork","Santiago","Texas","Indiana"))
df2

#inner_join
df_ij1 = inner_join(df1,df2,by='CustomerId')
df_ij1

df_ij2 = df1%>%inner_join(df2, by='CustomerId') #โยน df1 to df2
df_ij2

#outer_join
df_U = full_join(df1,df2,by='CustomerId')
df_U

#left join
df_L = left_join(df1,df2,by='CustomerId') #เก็บค่าของทางซ้ายทั้งหมดแล้วเก็บค่าของขวาที่เหมือนกัน
df_L
df_L2 = left_join(df2,df1,by='CustomerId')
df_L2

#right join
df_R = right_join(df1,df2,by='CustomerId')
df_R 
df_R2 = right_join(df2,df1,by='CustomerId')
df_R2

df_U %>% glimpse() #ดูตัวแปรๆ #%>%  โยนซ้ายมาขวา

glimpse(df_U)


#Check missing data
results = apply(is.na(df_L),2,which) #is.na contain the missing value

###Filter out the missing values by drop_na
library(tidyverse)
df_UC = drop_na(df_U) #ตัดแภวที่มี NA ออก
df_UC

