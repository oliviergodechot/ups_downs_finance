#------------------------------------------#
#Nils Neumann and Olivier Godechot
#contact: nils.neumann@sciencespo.fr
#Replication script for: Ups and downs in finance; ups without downs in inequality
#Figures
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
library(RColorBrewer)
library(car)
library(R2HTML)
library(ggplot2)
#------------------------------------------#
#####   some parameters and colors     #####
#------------------------------------------#
mystart = 1993
mytop = 2007
myend = 2013
mybottom = NA
darkcols = c(brewer.pal(8, "Dark2"),brewer.pal(8, "Paired"))
sw="Sweden"
cn="Canada"
fr="France"
no="Norway"
dk="Denmark"
jp="Japan"
de="Germany"
cz="Czechia"
kr="South Korea"
nl="Netherlands"
hu="Hungary"
us="USA (CPS)"
es="Spain"
Sweden = c(brewer.pal(8, "BuGn")[7],"1","2")
Canada = c(brewer.pal(8, "Blues")[8],"1","1")
USA = c(brewer.pal(8, "Blues")[6],"1","13")
France = c(brewer.pal(8, "RdPu")[7],"1","0")
Norway = c(brewer.pal(8, "Accent")[1],"1","5")
Denmark = c(brewer.pal(8, "Dark2")[5],"1","6")
Japan = c(brewer.pal(8, "YlOrRd")[4],"2","9")
Germany = c(brewer.pal(8, "Dark2")[4],"2","7")
Czechia = c(brewer.pal(8, "BrBG")[1],"2","4")
SouthKorea = c(brewer.pal(8, "YlOrRd")[5],"2","3")
Hungary = c(brewer.pal(8, "BrBG")[2],"2","10")
Netherlands = c(brewer.pal(8, "RdPu")[3],"1","15")
Spain = c(brewer.pal(8, "Set1")[1],"2","11")
#---------------------------------#
#####       functions         #####
#---------------------------------#
odds = function(x)
{
  odds = (x/(1-x))
  return(odds)
}
invodds = function(x)
{
  invodds = (x/(1+x))
  return(invodds)
}
id = function(x)
{
  id = x*1
  return(id)
}
logodds = function(x)
{
  logodds = log(x/(1-x))
  return(logodds)
}
invlogodds = function(x)
{
  invlogodds = exp(x)/(1+exp(x))
  return(invlogodds)
}
oddsratio = function(x,y)
{
  or = (x/(1-x))/(y/(1-y))
  return(or)
}
ctrycolor = data.frame(Canada,Czechia,Denmark,France,Germany,Hungary,Japan,Netherlands,Norway,SouthKorea,Spain,Sweden,USA)
colnames(ctrycolor)[which(names(ctrycolor) %in% "SouthKorea")] = "South Korea"
logodds100 = function(x)
{
  logodds = log(x/(100-x))
  return(logodds)
}
mygraph_f  = function(myvar,mytitle,my_y,mylog,ratio="logodds",legpos="topleft",average=1,myyaxt="s",
                     my_mar=c(2.5,4,2.5,1),my_inset=c(0,0.1),my_legend="y",myxaxt="s",my_ylim=NULL,my_xlim=NULL) {    
  m = get(substr(deparse(substitute(myvar)),1,-1+regexpr("\\$",deparse(substitute(myvar)))))
  m$dv = myvar
  g = as.data.frame(tapply(myvar,list(m$year,m$country),mean))
  if (ratio=="OR" & myyaxt=="n") {myfunc = logodds100} else  {myfunc = id}
  g$empty = NA
  delcol = NULL
  for (i in 1:ncol(g)) {
    if(sum(is.na(g[,i]))>=nrow(g)-1) { delcol  = c(delcol,i)}
  }
  g = g[,-delcol]
  if (ratio=="OR") {
    g = 100*g
    percent = "%"
  } else {percent = ""}
  g$year = rownames(g)
  
  year = data.frame(c(min(g$year):max(g$year)))
  colnames(year) = "year"
  rownames(year) = year$year
  g = merge(year,g,by="year",all.x=TRUE)
  rownames(g) = year$year
  g2 = g
  for (i in 1:ncol(g)) {
    if(sum(is.na(g[,i]))<nrow(g)-1) { g2[,i]  = na.approx(g[,i],rownames(g),na.rm=FALSE)}
  }
  g2dif = g2[2:nrow(g2),]-g2[1:nrow(g2)-1,]
  
  if (average==3)
  { 
    h = (g2+rbind(rep(NA,ncol(g2)),g2[1:nrow(g2)-1,])+rbind(g2[2:nrow(g2),],rep(NA,ncol(g2))))/3
  } else
  {
    h = g2
  }
  h2 = h
  h2dif = h2[2:nrow(h2),]-h2[1:nrow(h2)-1,]
  
  y = (1-is.na(h))*h$year
  y[y==0] = NA
  y = as.data.frame(y)
  mystart = sapply(y,min,na.rm=TRUE)
  myend = sapply(y,max,na.rm=TRUE)
  first0 = (is.na(h)==FALSE & is.na(rbind(rep(NA,ncol(h)),h[1:nrow(h)-1,])==TRUE))*h
  first0[first0==0] = NA
  first = sapply(first0,max,na.rm=TRUE)
  last0 = (is.na(h)==FALSE & is.na(rbind(h[2:nrow(h),],rep(NA,ncol(h))))==TRUE)*h
  last0[last0==0] = NA
  last = sapply(last0,max,na.rm=TRUE)
  
  if (ratio=="OR") 
  {
    my_ratio = exp(log(oddsratio(last/100,first/100))/(myend-mystart))
  } else if (ratio=="logodds") 
  {
    
    my_ratio = exp((last-first)/(myend-mystart))
    
  }  else if (ratio=="simple") 
  {
    
    my_ratio = exp(log(last/first)/(myend-mystart))
    
  }
  
  yratio = round(my_ratio,4)
  ylogratio = round(log(my_ratio),4)
  gr_rate = round((my_ratio-1)*100,2)
  
  
  g3 = data.frame(year,rowMeans(h2[,-1],na.rm=TRUE))
  colnames(g3)[2] = "mean_or"
  g3$ldif = c(NA,rowMeans(h2dif[,-1],na.rm=TRUE))
  g3$nbval = ncol(h2[,-1])-rowSums(is.na(h2[,-1]))
  g3$maxnbval = max(g3$nbval)
  g3$atmax = (g3$nbval>=g3$maxnbval)*1
  g3$aftermax = (cumsum(g3$atmax)>=1)*1
  g3$meanco = ifelse(g3$atmax,g3$mean_or,NA)
  
  for (i in 1:nrow(g3)) {
    if(is.na(g3$meanco[i])==TRUE & g3$aftermax[i]==1)  {g3$meanco[i] = g3$meanco[i-1]+g3$ldif[i]}
  }
  g3 = g3[order(rev(sort(g3$year))),]
  for (i in 1:nrow(g3)) {
    if(is.na(g3$meanco[i])==TRUE & g3$aftermax[i]==0)  {g3$meanco[i] = g3$meanco[i-1]-g3$ldif[i-1]}
  }
  g3 = g3[order(g3$year),]
  g3$meanco
  g3$meanco2 = ifelse(g3$nbval>2,g3$meanco,NA)
  
  bottom = min(g3$year[g3$nbval>2])
  top = max(g3$year[g3$nbval>2])
  
  if (ratio=="OR") {
    r = (g3$meanco[g3$year==top]/(100-g3$meanco[g3$year==top]))/(g3$meanco[g3$year==bottom]/(100-g3$meanco[g3$year==bottom]))
    r = (exp(log(r)/(top-bottom))-1)*100
  }  else if (ratio=="logodds") {
    r = g3$meanco[g3$year==top]-g3$meanco[g3$year==bottom]
    r = (exp(r/(top-bottom))-1)*100
    if (myyaxt=="n") {g3$meanco = exp(g3$meanco)}  
  } else if (ratio=="simple") {
    r = g3$meanco[g3$year==top]/g3$meanco[g3$year==bottom]
    r = (exp(log(r)/(top-bottom))-1)*100
  }
  
  mycol = as.vector(na.omit(t(ctrycolor[1,which(names(ctrycolor) %in% colnames(g2[-1]))])))
  mylty = as.vector(as.numeric(na.omit(t(ctrycolor[2,which(names(ctrycolor) %in% colnames(g2[-1]))]))))
  mypch = as.vector(as.numeric(na.omit(t(ctrycolor[3,which(names(ctrycolor) %in% colnames(g2[-1]))]))))
  
  par(xpd=FALSE, mar=my_mar)
  # par(oma=c(0,0,0,0))
  matplot(g$year,myfunc(g[,2:ncol(g)]),pch=mypch,col=mycol,ylab=my_y,xlab="",cex=0.7,las = 2, cex.lab=0.7, 
          cex.axis=0.7, log=mylog,bg=NA,yaxt=myyaxt,xaxt=myxaxt,xlim=my_xlim,
          ylim=my_ylim)
  if (myxaxt!="n") 
  {  
    axis(1, at = c(1971:1974,1976:1979,1981:1984,1986:1989,1991:1994,1996:1999,2001:2004,2006:2009,2011:2014,2016:2019,6),las=2,cex.axis=0.7)
  }
  matlines(g2$year,myfunc(cbind(g2[,2:ncol(g)])),lty=mylty,col=mycol)
  matlines(g3$year,myfunc(g3$meanco2),lwd = 3,col="black")
  if (ratio=="logodds" & myyaxt=="n")   
  {
    scale = c(1/10,1/9,1/8,1/7,1/6,1/5,1/4,1/3,1/2.5,1/2,1/1.8,1/1.6,1/1.4,1/1.2,1,1.2,1.4,1.6,1.8,2,2.5,3,4,5,6,7,8,9,10)
    scalelabel = c("/10","/9","/8","/7","/6","/5","/4","/3","/2.5","/2",
                  "/1.8","/1.6","/1.4","/1.2","1","× 1.2","× 1.4","× 1.6","× 1.8",
                  "× 2","× 2.5","× 3","× 4","× 5","× 6","× 7","× 8","× 9","× 10")
    
    axis(2,log(scale),label =scalelabel, las=2,cex.axis=0.8)  
  } else if (ratio=="OR" & myyaxt=="n")   
  {
    prop = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,
            1,2,3,4,5,6,7,8,9,
            10,12,14,16,18,20,25,30,35,40,45,50,55,60,65,70,75,80,82,84,86,88,90,
            91,92,93,94,95,96,97,98,99,
            99.1,99.2,99.3,99.4,99.5,99.6,99.7,99.8,99.9)
    axis(2,logodds100(prop),label = paste(prop,"%"), las=2,cex.axis=0.8)
  } 
  
  par(xpd=TRUE)
  if (my_legend!="n")
  {    
    legend(legpos, 
           inset=my_inset,
           legend=c(colnames(g[,-1]),"Adj. Mean"),pch =c(mypch,NA), 
           col=c(mycol,"black"),cex=0.8,ncol=6,lty=c(mylty,1),lwd = c(rep(1,ncol(g)-1),3),
           text.col = c(mycol,"black"),bg=NA,box.col="gray",box.lty=0)
  }
  c(g3$meanco[g3$year==bottom], g3$meanco[g3$year==top], g3$meanco[g3$year==top]/g3$meanco[g3$year==bottom])
  if (mytitle!="") {
    title(main=mytitle,sub=paste(ncol(g)-1,"countries.",bottom,":",round(g3$meanco[g3$year==bottom],1),percent,";",top,":", round(g3$meanco[g3$year==top],1),percent,";",
                                 "Yearly growth rate :",round(r,1),"%"))
  }
  if (r>0){plus = "+"
  }  else {plus = ""}
  mydelta = paste("\n",plus,round(r,1),"% / year")
  
  if (ratio=="logodds") {  
    percent2 = ""
    myfunc2 = exp
    cross = "×"
  }   else if (ratio=="OR") {
    percent2 = "%"
    myfunc2 = id
    cross = ""
  }  else {
    percent2 = ""
    myfunc2 = id
    cross = ""
  }
  
  text(g3$year[g3$year %in% c(bottom,top)],myfunc(g3$meanco2[g3$year %in% c(bottom,top)]),
       labels=paste(g3$year[g3$year %in% c(bottom,top)],"\n",
                    cross,round(myfunc2(g3$meanco2[g3$year %in% c(bottom,top)]),1),percent2),
       pos=c(1,1),cex= 0.7)
  
  des = data.frame(mystart,first,myend,last,yratio,ylogratio,gr_rate)
  des$country = rownames(des)
  des1 = des[1,]
  des1$country = "Adj. Mean"
  des1$mystart = bottom
  des1$first = g3$meanco2[g3$year %in% c(bottom)]
  des1$myend = top
  des1$last = g3$meanco2[g3$year %in% c(top)]
  des1$gr_rate = round(r,2)
  des1$yratio = 1+r/100
  des1$ylogratio = log(des1$yratio)
  des = rbind.fill(des,des1)
  
  rownames(des) = des$country

  print(des)
  
  if (ratio=="OR")
  {
    ltr = lm(logodds(dv)~country+country:year,data=m)
    ltr2 = lm(logodds(dv)~country+country:year+country:year:I(year>2007),data=m)
    ltr_all = lm(logodds(dv)~country+year,data=m)
    ltr_all2 = lm(logodds100(meanco2)~year,data=g3)
    ltr_all3 = lm(logodds(dv)~country+year+year:I(year>2007),data=m)
    ltr_all4 = lm(logodds100(meanco2)~year+year:I(year>2007),data=g3)
    
    
  } else if (ratio=="simple")
  {
    ltr = lm(log(dv)~country+country:year,data=m)
    ltr2 = lm(log(dv)~country+country:year+country:year:I(year>2007),data=m)
    ltr_all = lm(log(dv)~country+year,data=m)
    ltr_all2 = lm(log(meanco2)~year,data=g3)
    ltr_all3 = lm(log(dv)~country+year+year:I(year>2007),data=m)
    ltr_all4 = lm(log(meanco2)~year+year:I(year>2007),data=g3)
    
  } else if (ratio=="logodds")
  {
    ltr = lm(dv~country+country:year,data=m)
    ltr2 = lm(dv~country+country:year+country:year:I(year>2007),data=m)
    ltr_all = lm(dv~country+year,data=m)
    ltr_all2 = lm(meanco2~year,data=g3)
    ltr_all3 = lm(dv~country+year+year:I(year>2007),data=m)
    ltr_all4 = lm(meanco2~year+year:I(year>2007),data=g3)
  }
  print(screenreg(list(ltr,ltr2,ltr_all,ltr_all2,ltr_all3,ltr_all4),digits=4))
  
  return(list(m=m,g=g,g2=g2,g3=g3,h2=h2,mycol=mycol,des=des
              ,ltr=ltr,ltr_all=ltr_all,ltr_all2=ltr_all2,mydelta=mydelta
  ))
}
#---------------------------------#
#####       load data         #####
#---------------------------------#
#set your working directory
df = read.csv("replication_data_earnings_and_finance.csv")
df$sh.F99100_fs0 = df$sh.F99100 - df$sh.F99100_fs1
#------------------------------------------------------------------------------------------------------------------#
#                                                                                                                  #
#####                                              Output                                                      #####
#                                                                                                                  #
#------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------#
##### Figure 1. Evolution of top 1% wage earnings in 13 countries #####
#---------------------------------------------------------------------#
myctry = colnames(ctrycolor)
mycol = as.vector(na.omit(t(ctrycolor[1,which(names(ctrycolor) %in% myctry)])))
mylty = as.vector(as.numeric(na.omit(t(ctrycolor[2,which(names(ctrycolor) %in% myctry)]))))
mypch = as.vector(as.numeric(na.omit(t(ctrycolor[3,which(names(ctrycolor) %in% myctry)]))))
myorder = c(1,13,NA,3,9,12,4,8,5,11,2,6,10,7)
myctry = myctry[myorder]
mycol = mycol[myorder]
mylty = mylty[myorder]
mypch = mypch[myorder]
df_a = df[df$year>1989,]
#PDF
pdf(file="figure_1.pdf")
  fig1 = mygraph_f(df_a$sh.F99100_ps,"","Share of the national wage bill (log-odds scale)","",ratio="OR",myyaxt="n",my_legend = "n",average=1)
  legend("topleft", 
         inset=c(0,-0.1),
         legend=c(myctry,"Adjust. Mean"),pch =c(mypch,NA), 
         col=c(mycol,"black"),cex=0.8,ncol=6,lty=c(mylty,1),lwd = c(rep(1,ncol(fig1$g)),3),
         text.col = c(mycol,"black"),bg=NA,box.col="gray",box.lty=0)
dev.off()
#-------------------------------------------------------------------------------------#
##### Figure 2. Overrepresentation of financiers’ earnings in the national top 1% #####
#-------------------------------------------------------------------------------------#
df_b = df[df$year>1989,]
df_b = df_b[(df_b$country %in% "USA")==F,]
myctryb = myctry[c(1,NA,NA,4:14)]
mycolb = mycol[c(1,NA,NA,4:14)]
myltyb = mylty[c(1,NA,NA,4:14)]
mypchb = mypch[c(1,NA,NA,4:14)]
#PDF
pdf(file="figure_2.pdf")
  fig2 = mygraph_f(df_b$osh.F99100_fs1,"","Odds ratio","y",ratio="simple",myyaxt="s",my_legend = "n",average=1)
  legend("topleft", 
         inset=c(0,-0.1),
         legend=c(myctryb,"Adjust. Mean"),pch =c(mypchb,NA), 
         col=c(mycolb,"black"),cex=0.8,ncol=6,lty=c(myltyb,1),lwd = c(rep(1,ncol(fig2$g2)+1),3),
         text.col = c(mycolb,"black"),bg=NA,box.col="gray",box.lty=0)
dev.off()
#----------------------------------------------------------------#
##### Figure 3. Volumes of trade on national stock-exchanges #####
#----------------------------------------------------------------#
df_c = df[df$year>1988 & df$year<2019,]
df_c$year = df_c$year-1 #because this is a lagged variable in the df we do year -1
df_c$volgdp_g = df_c$GFDD.DM.02  
df_c$volgdp_g[df_c$country %in% "Czechia" & df_c$year<1995] = NA
df_c$volgdp_g[df_c$country %in% "Hungary" & df_c$year<1997] = NA
#PDF
pdf(file="figure_3.pdf")
a = mygraph_f(df_c$volgdp_g,"","Share of PIB (percentage-logaritmic scale)","y",ratio="simple",my_legend = "n" , average = 1)
legend("topleft", 
       inset=c(0,-0.1),
       legend=c(myctry,"Adjust. Mean"),pch =c(mypch,NA), 
       col=c(mycol,"black"),cex=0.8,ncol=6,lty=c(mylty,1),lwd = c(rep(1,length(myctry)),3),
       text.col = c(mycol,"black"),bg=NA,box.col="gray",box.lty=0)
dev.off()
#---------------------------------------#
##### Figure 4. Bank capital ratios #####
#---------------------------------------#
df_d = df
df_d$year = df_d$year-1 #because this is a lagged variable in the df we do year -1 
df_d$capgdp_g = df_d$GFDD.DM.01
df_d$capgdp_g[df_d$country %in% "Czechia" & df_d$year<1995] = NA
df_d$capgdp_g[df_d$country %in% "Hungary" & df_d$year<1997] = NA
df_d = df_d[df_d$year>1997 & df_d$year<2018,]
#PDF
pdf(file="figure_4.pdf")
  par(mfrow=c(2,1))
  f4a = mygraph_f(df_d$GFDD.SI.03,"","Percentage of assets (logaritmic scale)","y",ratio="simple",my_legend = "n"
               ,my_mar=c(0.1,4,3,1),myxaxt = "n",average=1)
  legend("topleft", 
         inset=c(0,-0.2),
         legend=c(myctry,"Adjust. Mean"),pch =c(mypch,NA), 
         col=c(mycol,"black"),cex=0.8,ncol=6,lty=c(mylty,1),lwd = c(rep(1,length(myctry)),3),
         text.col = c(mycol,"black"),bg=NA,box.col="gray",box.lty=0)
  text(2017, 3, "Bank capital to total assets (%)",
       cex = .8, pos=2)
  f4b = mygraph_f(df_d$GFDD.SI.05,"","Percentage of risk weighted assets (logaritmic scale)","y",ratio="simple",my_legend = "n",
               my_mar=c(2.5,4,0.1,1),average=1)
  text(2017, 9, "Bank regulatory capital to risk-weighted assets (%)",
       cex = .8, pos=2)
dev.off()
#--------------------------------------------------------------------------------------------------------------#
##### Figure A1. Evolution of top earnings shares of financiers and non-financiers in the national top 1%  #####
#--------------------------------------------------------------------------------------------------------------#
df_e = df[df$country %in% "USA"==F,]
df_e = df_e[df_e$year>1989,]
#PDF
pdf(file="figure_A1.pdf")
  par(mfrow=c(2,1))
  f4a = mygraph_f(df_e$sh.F99100_fs1,"","Share of national wage bill for financiers in top1%","y",ratio="OR",my_legend = "n"
                 ,my_mar=c(0.1,4,3,1),myxaxt = "n",average=1)
  legend("topleft", 
         inset=c(0,-0.2),
         legend=c(myctryb,"Adjust. Mean"),pch =c(mypchb,NA), 
         col=c(mycolb,"black"),cex=0.8,ncol=6,lty=c(myltyb,1),lwd = c(rep(1,length(myctryb)+1),3),
         text.col = c(mycolb,"black"),bg=NA,box.col="gray",box.lty=0)
  text(2017, 3, "",
       cex = .8, pos=2)
  f4b = mygraph_f(df_e$sh.F99100_fs0,"","Share of national wage bill for non-financiers in top1%","y",ratio="OR",my_legend = "n",
                 my_mar=c(2.5,4,0.1,1),average=1)
  text(2017, 9, "",
       cex = .8, pos=2)
dev.off()
#---------------------------------------------------------#
##### Figure A2. Banks’ Return on Equity before tax   #####
#---------------------------------------------------------#
df_f = df
df_f$year = df_f$year-1 #because this is a lagged variable in the df we do year -1 
df_f = df_f[df_f$year>1997 & df_f$year<2018,]
#PDF
pdf(file="figure_A2.pdf")
a = mygraph_f(df_f$GFDD.EI.10,"","ROE before tax (percentage)","",ratio="simple",my_legend = "n",average=1)
legend("topleft", 
       inset=c(0,-0.1),
       legend=c(myctry,"Adjust. Mean"),pch =c(mypch,NA), 
       col=c(mycol,"black"),cex=0.8,ncol=6,lty=c(mylty,1),lwd = c(rep(1,length(myctry)),3),
       text.col = c(mycol,"black"),bg=NA,box.col="gray",box.lty=0)
dev.off()
#-------------------------------------------#
##### Figure A3. Capitalization to GDP  #####
#-------------------------------------------#
df_g = df
df_g$year = df_g$year-1 #because this is a lagged variable in the df we do year -1 
df_g$capgdp_g = df_g$GFDD.DM.01
df_g$capgdp_g[df_g$country %in% "Czechia" & df_g$year<1995] = NA
df_g$capgdp_g[df_g$country %in% "Hungary" & df_g$year<1997] = NA
df_g = df_g[df_g$year>1988 & df_g$year<2019,]
#PDF
pdf(file="figure_A3.pdf")
a = mygraph_f(df_g$capgdp_g,"","Share of GDP (percentage-logaritmic scale)","y",ratio="simple",my_legend = "n")
legend("topleft", 
       inset=c(0,-0.1),
       legend=c(myctry,"Adjust. Mean"),pch =c(mypch,NA), 
       col=c(mycol,"black"),cex=0.8,ncol=6,lty=c(mylty,1),lwd = c(rep(1,length(myctry)),3),
       text.col = c(mycol,"black"),bg=NA,box.col="gray",box.lty=0)
dev.off()
#------------------------------------------------------------------------------------------------------------------#
#                                                                                                                  #
#####                                              End                                                         #####
#                                                                                                                  #
#------------------------------------------------------------------------------------------------------------------#