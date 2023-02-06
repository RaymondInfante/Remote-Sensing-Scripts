#Analyze GER1500 Data 
library(tidyverse)
#Attach file at the R code Document: 
#call in multiple csv files
#Get list of file
list_csv_file <- list.files(path = '/Users/raymondinfante/Documents/Coding/R code/Remote Sensing/GER 1500 5 de Octubre', full.names = T)
#Creat Function for reading all the csv
read_csv_files <-  function(file) {
  df <- read_table(file, col_names = F)
  return(df)
}

#Reading all csv with purrr and Merging all the csv
df <- purrr::map(.x = list_csv_file, .f = read_csv_files)

df1 <- as.data.frame(df)
view(df1)
head(df)


#Separar los datos del GER en lo,Ls y Ed Por estaciones.)
df_lo_TUR <- df1 %>% 
  select(X1,X2,X2.1,X2.2,X2.3,X2.4,X2.5,X2.6,X2.7,X2.8,X2.9) %>% 
  mutate(mean_lo = (X2+X2.1+X2.2+X2.3+X2.4+X2.5+X2.6+X2.7+X2.8+X2.9)/10) %>% 
  select(X1,mean_lo)
#Verificar que todos los plots esten en el mismo range. Que no exista outliers
plot(df1$X1,df1$X2, type = 'l')
lines(df1$X1,df1$X2.1, type = 'l')
lines(df1$X1,df1$X2.2, type = 'l')
lines(df1$X1,df1$X2.3, type = 'l')
lines(df1$X1,df1$X2.4, type = 'l')
lines(df1$X1,df1$X2.5, type = 'l')
lines(df1$X1,df1$X2.6, type = 'l')
lines(df1$X1,df1$X2.7, type = 'l')
lines(df1$X1,df1$X2.8, type = 'l')
lines(df1$X1,df1$X2.9, type = 'l')

df_ls_TUR <- df1 %>% 
  select(X1,X2.10, X2.11,X2.12, X2.13, X2.14, X2.15, X2.16, X2.17, X2.18, X2.19) %>% 
  mutate(mean_ls = (X2.10+ X2.11+X2.12+ X2.13+ X2.14+ X2.15+ X2.16+ X2.17+ X2.18+ X2.19)/10) %>% 
  select(mean_ls)

plot(df1$X1,df1$X2.10, type = 'l')
lines(df1$X1,df1$X2.11, type = 'l')
lines(df1$X1,df1$X2.12, type = 'l')
lines(df1$X1,df1$X2.13, type = 'l')
lines(df1$X1,df1$X2.14, type = 'l')
lines(df1$X1,df1$X2.15, type = 'l')
lines(df1$X1,df1$X2.16, type = 'l')
lines(df1$X1,df1$X2.17, type = 'l')
lines(df1$X1,df1$X2.18, type = 'l')
lines(df1$X1,df1$X2.19, type = 'l')

df_ed_TUR <- df1 %>% 
  select(X1,X2.20, X2.21,X2.22,X2.23,X2.24,X2.25,X2.26,X2.27,X2.28,X2.29) %>% 
  mutate(mean_ed = (X2.20+ X2.21+X2.22+ X2.23+ X2.24+ X2.25+ X2.26+ X2.27+ X2.28+ X2.29)/10) %>% 
  select(mean_ed)

plot(df1$X1,df1$X2.20, type = 'l')
lines(df1$X1,df1$X2.21, type = 'l')
lines(df1$X1,df1$X2.22, type = 'l')
lines(df1$X1,df1$X2.23, type = 'l')
lines(df1$X1,df1$X2.24, type = 'l')
lines(df1$X1,df1$X2.25, type = 'l')
lines(df1$X1,df1$X2.26, type = 'l')
lines(df1$X1,df1$X2.27, type = 'l')
lines(df1$X1,df1$X2.28, type = 'l')
lines(df1$X1,df1$X2.29, type = 'l')

#Juntar todos los means Lo,Ls, Ed
Means_TUR<- as.data.frame(c(df_lo_TUR, df_ls_TUR,df_ed_TUR))
head(Means_TUR)
#Create Rrs with formula
Rrs_TUR <- Means_TUR %>% 
  mutate(Rrs = (Means_TUR$mean_lo)-(0.028*Means_TUR$mean_ls)) %>% 
  mutate(Rrs_Final = Rrs/Means_TUR$mean_ed)
head(Rrs_TUR)

# Cayo Coral
df_lo_CC <- df1 %>% 
  select(X1,X2.30,X2.31,X2.32,X2.33,X2.34,X2.35,X2.36,X2.37,X2.38,X2.39) %>% 
  mutate(mean_lo = (X2.30+X2.31+X2.32+X2.33+X2.34+X2.35+X2.36+X2.37+X2.38+X2.39)/10) %>% 
  select(X1,mean_lo)

plot(df1$X1,df1$X2.30, type = 'l')
lines(df1$X1,df1$X2.31, type = 'l')
lines(df1$X1,df1$X2.32, type = 'l')
lines(df1$X1,df1$X2.33, type = 'l')
lines(df1$X1,df1$X2.34, type = 'l')
lines(df1$X1,df1$X2.35, type = 'l')
lines(df1$X1,df1$X2.36, type = 'l')
lines(df1$X1,df1$X2.37, type = 'l')
lines(df1$X1,df1$X2.38, type = 'l')
lines(df1$X1,df1$X2.39, type = 'l')

df_ls_CC <- df1 %>% 
  select(X1,X2.40, X2.41,X2.42, X2.43, X2.44, X2.45, X2.46, X2.47, X2.48, X2.49) %>% 
  mutate(mean_ls = (X2.40+ X2.41+X2.42+ X2.43+ X2.44+ X2.45+ X2.46+ X2.47+ X2.48+ X2.49)/10) %>% 
  select(mean_ls)

plot(df1$X1,df1$X2.40, type = 'l')
lines(df1$X1,df1$X2.41, type = 'l')
lines(df1$X1,df1$X2.42, type = 'l')
lines(df1$X1,df1$X2.43, type = 'l')
lines(df1$X1,df1$X2.44, type = 'l')
lines(df1$X1,df1$X2.45, type = 'l')
lines(df1$X1,df1$X2.46, type = 'l')
lines(df1$X1,df1$X2.47, type = 'l')
lines(df1$X1,df1$X2.48, type = 'l')
lines(df1$X1,df1$X2.49, type = 'l')

df_ed_CC <- df1 %>% 
  select(X1,X2.50, X2.51,X2.52,X2.53,X2.54,X2.55,X2.56,X2.57,X2.58,X2.59) %>% 
  mutate(mean_ed = (X2.50+ X2.51+X2.52+ X2.53+ X2.54+ X2.55+ X2.56+ X2.57+ X2.58+ X2.59)/10) %>% 
  select(mean_ed)

plot(df1$X1,df1$X2.50, type = 'l')
lines(df1$X1,df1$X2.51, type = 'l')
lines(df1$X1,df1$X2.52, type = 'l')
lines(df1$X1,df1$X2.53, type = 'l')
lines(df1$X1,df1$X2.54, type = 'l')
lines(df1$X1,df1$X2.55, type = 'l')
lines(df1$X1,df1$X2.56, type = 'l')
lines(df1$X1,df1$X2.57, type = 'l')
lines(df1$X1,df1$X2.58, type = 'l')
lines(df1$X1,df1$X2.59, type = 'l')

Means_CC<- as.data.frame(c(df_lo_CC, df_ls_CC,df_ed_CC))
head(Means_CC)
#Create Rrs with formula
Rrs_CC <- Means_CC %>% 
  mutate(Rrs = (Means_CC$mean_lo)-(0.028*Means_CC$mean_ls)) %>% 
  mutate(Rrs_Final = Rrs/Means_CC$mean_ed)
head(Rrs_CC)

#Bahia biol

df_lo_BAY <- df1 %>% 
  select(X1,X2.60,X2.61,X2.62,X2.63,X2.64,X2.65,X2.66,X2.67,X2.68,X2.69) %>% 
  mutate(mean_lo = (X2.60+X2.61+X2.62+X2.63+X2.64+X2.65+X2.66+X2.67+X2.68+X2.69)/10) %>% 
  select(X1,mean_lo)

plot(df1$X1,df1$X2.60, type = 'l')
lines(df1$X1,df1$X2.61, type = 'l')
lines(df1$X1,df1$X2.62, type = 'l')
lines(df1$X1,df1$X2.63, type = 'l')
lines(df1$X1,df1$X2.64, type = 'l')
lines(df1$X1,df1$X2.65, type = 'l')
lines(df1$X1,df1$X2.66, type = 'l')
lines(df1$X1,df1$X2.67, type = 'l')
lines(df1$X1,df1$X2.68, type = 'l')
lines(df1$X1,df1$X2.69, type = 'l')

df_ls_BAY <- df1 %>% 
  select(X1,X2.70, X2.71,X2.72, X2.73, X2.74, X2.75, X2.76, X2.77, X2.78, X2.79) %>% 
  mutate(mean_ls = (X2.70+ X2.71+X2.72+ X2.73+ X2.74+ X2.75+ X2.76+ X2.77+ X2.78+ X2.79)/10) %>% 
  select(mean_ls)

plot(df1$X1,df1$X2.70, type = 'l')
lines(df1$X1,df1$X2.71, type = 'l')
lines(df1$X1,df1$X2.72, type = 'l')
lines(df1$X1,df1$X2.73, type = 'l')
lines(df1$X1,df1$X2.74, type = 'l')
lines(df1$X1,df1$X2.75, type = 'l')
lines(df1$X1,df1$X2.76, type = 'l')
lines(df1$X1,df1$X2.77, type = 'l')
lines(df1$X1,df1$X2.78, type = 'l')
lines(df1$X1,df1$X2.79, type = 'l')

df_ed_BAY <- df1 %>% 
  select(X1,X2.80, X2.81,X2.82,X2.83,X2.84,X2.85,X2.86,X2.87,X2.88,X2.89) %>% 
  mutate(mean_ed = (X2.80+ X2.81+X2.82+ X2.83+ X2.84+ X2.85+ X2.86+ X2.87+ X2.88+X2.89)/10) %>% 
  select(mean_ed)

plot(df1$X1,df1$X2.80, type = 'l')
lines(df1$X1,df1$X2.81, type = 'l')
lines(df1$X1,df1$X2.82, type = 'l')
lines(df1$X1,df1$X2.83, type = 'l')
lines(df1$X1,df1$X2.84, type = 'l')
lines(df1$X1,df1$X2.85, type = 'l')
lines(df1$X1,df1$X2.86, type = 'l')
lines(df1$X1,df1$X2.87, type = 'l')
lines(df1$X1,df1$X2.88, type = 'l')
lines(df1$X1,df1$X2.89, type = 'l')

Means_BAY<- as.data.frame(c(df_lo_BAY, df_ls_BAY,df_ed_BAY))
head(Means_BAY)
#Create Rrs with formula
Rrs_BAY <- Means_BAY %>% 
  mutate(Rrs = (Means_BAY$mean_lo)-(0.028*Means_BAY$mean_ls)) %>% 
  mutate(Rrs_Final = Rrs/Means_BAY$mean_ed)
head(Rrs_BAY)


# Cayo Enrique
df_lo_CE <- df1 %>% 
  select(X1,X2.90,X2.91,X2.92,X2.93,X2.94,X2.95,X2.96,X2.97,X2.98,X2.99) %>% 
  mutate(mean_lo = (X2.90+X2.91+X2.92+X2.93+X2.94+X2.95+X2.96+X2.97+X2.98+X2.99)/10) %>% 
  select(X1,mean_lo)

plot(df1$X1,df1$X2.90, type = 'l')
lines(df1$X1,df1$X2.91, type = 'l')
lines(df1$X1,df1$X2.92, type = 'l')
lines(df1$X1,df1$X2.93, type = 'l')
lines(df1$X1,df1$X2.94, type = 'l')
lines(df1$X1,df1$X2.95, type = 'l')
lines(df1$X1,df1$X2.96, type = 'l')
lines(df1$X1,df1$X2.97, type = 'l')
lines(df1$X1,df1$X2.98, type = 'l')
lines(df1$X1,df1$X2.99, type = 'l')

df_ls_CE <- df1 %>% 
  select(X1,X2.100, X2.101,X2.102, X2.103, X2.104, X2.105, X2.106, X2.107, X2.108, X2.109) %>% 
  mutate(mean_ls = (X2.100+ X2.101+X2.102+ X2.103+ X2.104+ X2.105+ X2.106+ X2.107+ X2.108+ X2.109)/10) %>% 
  select(mean_ls)

plot(df1$X1,df1$X2.100, type = 'l')
lines(df1$X1,df1$X2.101, type = 'l')
lines(df1$X1,df1$X2.102, type = 'l')
lines(df1$X1,df1$X2.103, type = 'l')
lines(df1$X1,df1$X2.104, type = 'l')
lines(df1$X1,df1$X2.105, type = 'l')
lines(df1$X1,df1$X2.106, type = 'l')
lines(df1$X1,df1$X2.107, type = 'l')
lines(df1$X1,df1$X2.108, type = 'l')
lines(df1$X1,df1$X2.109, type = 'l')

df_ed_CE <- df1 %>% 
  select(X1,X2.110, X2.111,X2.112,X2.113,X2.114,X2.115,X2.116,X2.117,X2.118,X2.119) %>% 
  mutate(mean_ed = (X2.110+ X2.111+X2.112+ X2.113+ X2.114+ X2.115+ X2.116+ X2.117+ X2.118)/10) %>% 
  select(mean_ed)

plot(df1$X1,df1$X2.110, type = 'l')
lines(df1$X1,df1$X2.111, type = 'l')
lines(df1$X1,df1$X2.112, type = 'l')
lines(df1$X1,df1$X2.113, type = 'l')
lines(df1$X1,df1$X2.114, type = 'l')
lines(df1$X1,df1$X2.115, type = 'l')
lines(df1$X1,df1$X2.116, type = 'l')
lines(df1$X1,df1$X2.117, type = 'l')
lines(df1$X1,df1$X2.118, type = 'l')
lines(df1$X1,df1$X2.119, type = 'l')

Means_CE<- as.data.frame(c(df_lo_CE, df_ls_CE,df_ed_CE))
head(Means_CE)
#Create Rrs with formula
Rrs_CE <- Means_CE %>% 
  mutate(Rrs = (Means_CE$mean_lo)-(0.028*Means_CE$mean_ls)) %>% 
  mutate(Rrs_Final = Rrs/Means_CE$mean_ed)
head(Rrs_CE)




#Plot the Rrs
plot(Rrs_TUR$X1, Rrs_TUR$Rrs_Final, type = 'l', col = 'purple', ylab = 'Rrs (Steradians -1)', xlab = 'Wavelenght (nm)', main = 'Rrs Comparison',
     ylim = c(-0.000001, 0.0030), xlim = c(350, 800))
lines(Rrs_CC$X1, Rrs_CC$Rrs_Final, type = 'l', col = 'green')
lines(Rrs_BAY$X1, Rrs_BAY$Rrs_Final, type = 'l', col = 'blue')
lines(Rrs_CE$X1, Rrs_CE$Rrs_Final, type = 'l', col = 'red')
legend('topright', legend = c('TUR', 'CC', 'BAY', 'CE'), col = c('purple','green', 'blue','red'),
       lty=1)



head(Rrs_TUR)
head(Rrs_CC)
head(Rrs_BAY)
head(Rrs_CE)

Rrs_table <- as.data.frame(c(Rrs_TUR, Rrs_CC, Rrs_BAY, Rrs_CE))
Rrs_table

write.csv(Rrs_table,"C:\\Users\\raymondinfante\\Documents\\Coding\\R code\\Rrs_13_Sept.csv", row.names = FALSE)

