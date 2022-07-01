#------------------------------------------#
#Nils Neumann and Olivier Godechot
#contact: nils.neumann@sciencespo.fr
#Replication script for: Ups and downs in finance; ups without downs in inequality
#Tables
#June 2022
#------------------------------------------#
#------------------------------------------------------------------------------------------------------------------#
#                                                                                                                  #
#####                                              Setup                                                       #####
#                                                                                                                  #
#------------------------------------------------------------------------------------------------------------------#
library(texreg)
library(lfe)
library(zoo)
library(plyr)
library(car)
library(R2HTML)
library(reshape2)
#---------------------------------#
#####       functions         #####
#---------------------------------#
simplelag  = function(x,by=NULL,mylag=1,outside=NA)
{
  myend = length(x)- mylag
  if (!is.null(by)) {
    lby = c(replicate(mylag,""),as.character(by[1:myend]))
    y0 = c(replicate(mylag,outside),x[1:myend])
    y = ifelse(as.character(by)==lby,y0,outside)
    
  }
  else {
    y = c(replicate(mylag,outside),x[1:myend])
  }
}
std_dem = function(df, x, c)
{
  df$x_dem = (x-ave(x,c,FUN=function(y) mean(y,na.rm=T)))
  df$x_std = (df$x_dem/sd(df$x_dem,na.rm=T))
  return(df$x_std)
}
cum_drop = function(df, x, c)
{
  df$x_cum_drop_0 = (x-simplelag(x,by=c,mylag=1))*(x-simplelag(x,by=c,mylag=1)<0)
  df$x_cum_drop[is.na(df$x_cum_drop_0)==F] = ave(df$x_cum_drop_0[is.na(df$x_cum_drop_0)==F],df$country[is.na(df$x_cum_drop_0)==F],FUN=function(x) cumsum(x))
  df$x_cum_drop = ifelse(is.na(df$x_cum_drop)==T & is.na(x)==F,0,df$x_cum_drop)
  return(df$x_cum_drop)
}
#---------------------------------#
#####       load data         #####
#---------------------------------#
#set your working directory
df = read.csv("replication_data_earnings_and_finance.csv")
df$mysh.F99100_ps = 100*df$sh.F99100_ps
#-------------------------------------------#
##### Standardize and demean variables  #####
#-------------------------------------------#
df_s = df[is.na(df$sh.F99100_ps)==F,]
df_s$sh.F99100_ps_std = std_dem(df_s, df_s$sh.F99100_ps, df_s$country)
df_s$osh.F99100_fs1_std = std_dem(df_s, df_s$osh.F99100_fs1, df_s$country)
df_s$gdp_per_std = std_dem(df_s, df_s$gdp_per, df_s$country)
df_s$unions_app_std = std_dem(df_s, df_s$unions_app, df_s$country)
df_s$imports_std = std_dem(df_s, df_s$imports, df_s$country)
df_s$Kap2Ass_std = std_dem(df_s, df_s$GFDD.SI.03, df_s$country)
df_s$volgdp2_std = std_dem(df_s, df_s$GFDD.DM.02, df_s$country)
df_s$RegK2WtdAss_std = std_dem(df_s, df_s$GFDD.SI.05, df_s$country)
df_s$capi_std = std_dem(df_s, df_s$GFDD.DM.01, df_s$country)
#-------------------------#
##### Cumulative drop #####
#-------------------------#
df_s$volgdp2_std_cum_drop = cum_drop(df_s, df_s$volgdp2_std, df_s$country)
df_s$capi_std_cum_drop = cum_drop(df_s, df_s$capi_std, df_s$country)
#------------------------------------------------------------------------------------------------------------------#
#                                                                                                                  #
#####                                              Output                                                      #####
#                                                                                                                  #
#------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------#
##### Table 1. Finance contribution to the pre-financial crisis upswing in inequality and to the financial crisis downswing in inequality #####
#---------------------------------------------------------------------------------------------------------------------------------------------#
df$minboom[df$country %in% c("Canada")] = 1992
df$minboom[df$country %in% c("Czechia")] = 2003
df$minboom[df$country %in% c("Denmark")] = 1994
df$minboom[df$country %in% c("France")] = 1993
df$minboom[df$country %in% c("Germany")] = 1992
df$minboom[df$country %in% c("Hungary")] = 2003
df$minboom[df$country %in% c("Japan")] = 1997
df$minboom[df$country %in% c("Netherlands")] = 2006
df$minboom[df$country %in% c("Norway")] = 1996
df$minboom[df$country %in% c("South Korea")] = 1995
df$minboom[df$country %in% c("Spain")] = 2006
df$minboom[df$country %in% c("Sweden")] = 1990
df$minboom[df$country %in% c("USA")] = 1992

df$maxboom[df$country %in% c("Canada")] = 2006
df$maxboom[df$country %in% c("Czechia")] = 2008
df$maxboom[df$country %in% c("Denmark")] = 2008
df$maxboom[df$country %in% c("France")] = 2007
df$maxboom[df$country %in% c("Germany")] = 2008
df$maxboom[df$country %in% c("Hungary")] = 2005
df$maxboom[df$country %in% c("Japan")] = 2007
df$maxboom[df$country %in% c("Netherlands")] = 2007
df$maxboom[df$country %in% c("Norway")] = 2007
df$maxboom[df$country %in% c("South Korea")] = 2008
df$maxboom[df$country %in% c("Spain")] = 2007
df$maxboom[df$country %in% c("Sweden")] = 2007
df$maxboom[df$country %in% c("USA")] = 2002

df$maxcrash[df$country %in% c("Canada")] = 2006
df$maxcrash[df$country %in% c("Czechia")] = 2009
df$maxcrash[df$country %in% c("Denmark")] = 2008
df$maxcrash[df$country %in% c("France")] = 2007
df$maxcrash[df$country %in% c("Germany")] = 2008
df$maxcrash[df$country %in% c("Hungary")] = 2009
df$maxcrash[df$country %in% c("Japan")] = 2009
df$maxcrash[df$country %in% c("Netherlands")] = 2007
df$maxcrash[df$country %in% c("Norway")] = 2007
df$maxcrash[df$country %in% c("South Korea")] = 2008
df$maxcrash[df$country %in% c("Spain")] = 2007
df$maxcrash[df$country %in% c("Sweden")] = 2007
df$maxcrash[df$country %in% c("USA")] = 2002

df$mincrash[df$country %in% c("Canada")] = 2019
df$mincrash[df$country %in% c("Czechia")] = 2010
df$mincrash[df$country %in% c("Denmark")] = 2009
df$mincrash[df$country %in% c("France")] = 2013
df$mincrash[df$country %in% c("Germany")] = 2014
df$mincrash[df$country %in% c("Hungary")] = 2017
df$mincrash[df$country %in% c("Japan")] = 2010
df$mincrash[df$country %in% c("Netherlands")] = 2009
df$mincrash[df$country %in% c("Norway")] = 2018
df$mincrash[df$country %in% c("South Korea")] = 2010
df$mincrash[df$country %in% c("Spain")] = 2017
df$mincrash[df$country %in% c("Sweden")] = 2013
df$mincrash[df$country %in% c("USA")] = 2008

boom = data.frame(df$country[df$year==df$minboom],
                   paste(df$minboom[df$year==df$minboom],df$maxboom[df$year==df$minboom],sep="-"),
                   df$sh.F99100[df$year==df$minboom],
                   df$sh.F99100[df$year==df$maxboom],
                   (df$sh.F99100[df$year==df$maxboom]-df$sh.F99100[df$year==df$minboom])/
                     (df$year[df$year==df$maxboom]-df$year[df$year==df$minboom]),
                   df$sh.F99100_fs1[df$year==df$minboom],
                   df$sh.F99100_fs1[df$year==df$maxboom],
                   (df$sh.F99100_fs1[df$year==df$maxboom]-df$sh.F99100_fs1[df$year==df$minboom])/
                     (df$year[df$year==df$maxboom]-df$year[df$year==df$minboom]),
                   (df$sh.F99100_fs1[df$year==df$maxboom]-df$sh.F99100_fs1[df$year==df$minboom])/
                     (df$sh.F99100[df$year==df$maxboom]-df$sh.F99100[df$year==df$minboom]))
colnames(boom) = c("ctry","years","i_min","i_max","i_dif","f_min","f_max","f_dif","f_contr")

crash = data.frame(df$country[df$year==df$maxcrash],
                    paste(df$maxcrash[df$year==df$maxcrash],df$mincrash[df$year==df$maxcrash],sep="-"),
                    df$sh.F99100[df$year==df$maxcrash],
                    df$sh.F99100[df$year==df$mincrash],
                    (df$sh.F99100[df$year==df$mincrash]-df$sh.F99100[df$year==df$maxcrash])/
                      (df$year[df$year==df$mincrash]-df$year[df$year==df$maxcrash]),
                    df$sh.F99100_fs1[df$year==df$maxcrash],
                    df$sh.F99100_fs1[df$year==df$mincrash],
                    (df$sh.F99100_fs1[df$year==df$mincrash]-df$sh.F99100_fs1[df$year==df$maxcrash])/
                      (df$year[df$year==df$mincrash]-df$year[df$year==df$maxcrash]),
                    (df$sh.F99100_fs1[df$year==df$mincrash]-df$sh.F99100_fs1[df$year==df$maxcrash])/
                      (df$sh.F99100[df$year==df$mincrash]-df$sh.F99100[df$year==df$maxcrash]))
colnames(crash) = c("ctry","years","i_min","i_max","i_dif","f_min","f_max","f_dif","f_contr")

table_1 = merge(boom[,-c(6,7)],crash[,-c(3,6,7)],by="ctry")
table_1 = table_1[order(table_1$i_dif.x),]

unlink("Table 1.html")
HTML(table_1,file="Table 1.html")
#--------------------------------------------------------------------------------------------------------------#
##### Table 2. Linear trends in top 1% shares and overrepresentation of financiers’ earnings in the latter #####
#--------------------------------------------------------------------------------------------------------------#
df_a = df[(df$year>1989) %in% T,]
df_b = df_a[df_a$country != "USA",]
df_b$country[df_b$country == "USA CPS"] = "USA"
df_b$sh.F99100_fs1[df_b$country=="Germany" & df_b$year==1998] = NA #Outlier
df_b$osh.F99100_fs1[df_b$country=="Germany" & df_b$year==1998] = NA #Outlier

tt2_1a = felm(mysh.F99100_ps~year|country|0|0,data=df_a[df_a$year <= 2007 ,])

tt2_2a = felm(mysh.F99100_ps~year|country|0|0,data=df_a[df_a$year >= 2007,])

tt2_3a = felm(I(100*sh.F99100_fs1)~year|country|0|0,data=df_b[df_b$year <= 2007 & df_b$country %in% c("USA") ==F,])

tt2_4a = felm(I(100*sh.F99100_fs1)~year|country|0|0,data=df_a[df_a$year >= 2007 & df_a$country %in% "USA" ==F,])

tt2_5a = felm(osh.F99100_fs1~year|country|0|0,data=df_a[df_a$year <= 2007 & df_a$country %in% c("USA") ==F,])

tt2_6a = felm(osh.F99100_fs1~year|country|0|0,data=df_a[df_a$year >= 2007 & df_a$country %in% "USA" ==F,])

screenreg(list(tt2_1a, tt2_2a,tt2_3a,tt2_4a,tt2_5a,tt2_6a),
          custom.model.names=c("Top1% <=2007","Top1% >=2007","FiTop1% <=2007","FiTop1% >=2007","Fin OR <=2007","Fin OR >=2007"),
          digits=3,
          stars=c(0.1,0.05,0.01))
htmlreg(list(tt2_1a, tt2_2a,tt2_3a,tt2_4a,tt2_5a,tt2_6a),
        custom.model.names=c("Top1% <=2007","Top1% >=2007","FiTop1% <=2007","FiTop1% >=2007","Fin OR <=2007","Fin OR >=2007"),
        digits = 3,stars=c(0.1,0.05,0.01),
        file="Table 2a.html")

tt2_1 = felm(mysh.F99100_ps~country:year|country|0|0,data=df_a[df_a$year <= 2007 ,])

tt2_2 = felm(mysh.F99100_ps~country:year|country|0|0,data=df_a[df_a$year >= 2007,])

tt2_3 = felm(I(100*sh.F99100_fs1)~country:year|country|0|0,data=df_b[df_b$year <= 2007 & df_b$country %in% c("USA") ==F,])

tt2_4 = felm(I(100*sh.F99100_fs1)~country:year|country|0|0,data=df_a[df_a$year >= 2007 & df_a$country %in% "USA" ==F,])

tt2_5 = felm(osh.F99100_fs1~country:year|country|0|0,data=df_a[df_a$year <= 2007 & df_a$country %in% c("USA") ==F,])

tt2_6 = felm(osh.F99100_fs1~country:year|country|0|0,data=df_a[df_a$year >= 2007 & df_a$country %in% "USA" ==F,])

screenreg(list(tt2_1, tt2_2,tt2_3,tt2_4,tt2_5,tt2_6),
          custom.model.names=c("Top1% <=2007","Top1% >=2007","FiTop1% <=2007","FiTop1% >=2007","Fin OR <=2007","Fin OR >=2007"),
        digits = 3,stars=c(0.1,0.05,0.01))
htmlreg(list(tt2_1, tt2_2,tt2_3,tt2_4,tt2_5,tt2_6),
        custom.model.names=c("Top1% <=2007","Top1% >=2007","FiTop1% <=2007","FiTop1% >=2007","Fin OR <=2007","Fin OR >=2007"),
        digits = 3,stars=c(0.1,0.05,0.01),
        file="Table 2b.html")
#----------------------------------------------------------------------------------------------------------------------------------------#
##### Table 3. The asymmetric impact of trading volume on inequality and financial earnings overrepresentation in top earnings share #####
#----------------------------------------------------------------------------------------------------------------------------------------#
tt3_1 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+volgdp2_std|country+year|0|year,data=df_s)

tt3_2 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+volgdp2_std+volgdp2_std:I(year %in% c(2001:2003,2008:2013))|country+year|0|year,data=df_s)

tt3_3 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+volgdp2_std+volgdp2_std_cum_drop|year+country|0|year,data=df_s)

tt3_4 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+volgdp2_std|country+year|0|year,data=df_s[df_s$country !="USA",])

tt3_5 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std++volgdp2_std+volgdp2_std:I(year %in% c(2001:2003,2008:2013))|country+year|0|year,data=df_s[df_s$country !="USA",])

tt3_6 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+volgdp2_std+volgdp2_std_cum_drop|year+country|0|year,data=df_s[df_s$country !="USA",])

screenreg(list(tt3_1,tt3_2,tt3_3,tt3_4,tt3_5,tt3_6),digits = 2,stars=c(0.1,0.05,0.01))
htmlreg(list(tt3_1,tt3_2,tt3_3,tt3_4,tt3_5,tt3_6),digits = 2,stars=c(0.1,0.05,0.01),file="Table 3.html")
#---------------------------------------------------------#
##### Table 4. Impact of capital ratios on inequality #####
#---------------------------------------------------------#
tt4a_1 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+Kap2Ass_std|country+year|0|year,data=df_s)

tt4a_2 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+Kap2Ass_std+Kap2Ass_std:I(year>2008)|country+year|0|year,data=df_s)

tt4a_3 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+Kap2Ass_std|country+year|0|year,data=df_s[df_s$country !="USA",])

tt4a_4 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+Kap2Ass_std+Kap2Ass_std:I(year>2008)|country+year|0|year,data=df_s[df_s$country !="USA",])

screenreg(list(tt4a_1,tt4a_2,tt4a_3,tt4a_4),digits = 2,stars=c(0.1,0.05,0.01))
htmlreg(list(tt4a_1,tt4a_2,tt4a_3,tt4a_4),digits = 2,stars=c(0.1,0.05,0.01),file="Table 4a.html")

tt4b_1 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+RegK2WtdAss_std|country+year|0|year,data=df_s)

tt4b_2 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+RegK2WtdAss_std+RegK2WtdAss_std:I(year>2008)|country+year|0|year,data=df_s)

tt4b_3 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+RegK2WtdAss_std|country+year|0|year,data=df_s[df_s$country !="USA",])

tt4b_4 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+RegK2WtdAss_std+RegK2WtdAss_std:I(year>2008)|country+year|0|year,data=df_s[df_s$country !="USA",])

screenreg(list(tt4b_1,tt4b_2,tt4b_3,tt4b_4),digits = 2,stars=c(0.1,0.05,0.01))
htmlreg(list(tt4b_1,tt4b_2,tt4b_3,tt4b_4),digits = 2,stars=c(0.1,0.05,0.01),file="Table 4b.html")
#------------------------------------------------------------------------------------------------------------------#
##### Table A3. The asymmetric effect of capitalization on inequality and finance’s contribution to inequality #####
#------------------------------------------------------------------------------------------------------------------#
ttA3_1 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+capi_std|country+year|0|year,data=df_s)

ttA3_2 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+capi_std+capi_std:I(year %in% c(2001:2003,2008:2013))|country+year|0|year,data=df_s)

ttA3_3 = felm(sh.F99100_ps_std~gdp_per_std+unions_app_std+imports_std+capi_std+capi_std_cum_drop|year+country|0|year,data=df_s)

ttA3_4 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+capi_std|country+year|0|year,data=df_s[df_s$country !="USA",])

ttA3_5 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+capi_std+capi_std:I(year %in% c(2001:2003,2008:2013))|country+year|0|year,data=df_s[df_s$country !="USA",])

ttA3_6 = felm(osh.F99100_fs1_std~gdp_per_std+unions_app_std+imports_std+capi_std+capi_std_cum_drop|year+country|0|year,data=df_s[df_s$country !="USA",])

screenreg(list(ttA3_1,ttA3_2,ttA3_3,ttA3_4,ttA3_5,ttA3_6),digits = 2,stars=c(0.1,0.05,0.01))
htmlreg(list(ttA3_1,ttA3_2,ttA3_3,ttA3_4,ttA3_5,ttA3_6),digits = 2,stars=c(0.1,0.05,0.01),file="table A3.html")
#------------------------------------------------------------------------------------------------------------------#
#                                                                                                                  #
#####                                              End                                                         #####
#                                                                                                                  #
#------------------------------------------------------------------------------------------------------------------#