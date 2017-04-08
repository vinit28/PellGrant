library(ggplot2)

eleven = as.data.frame(read.csv("2011-2012.csv"))
twelve = as.data.frame(read.csv("2012-2013.csv"))
thirteen = as.data.frame(read.csv("2013-2014.csv"))
salary = as.data.frame(read.csv("salaryData.csv"))
salaryNYC = as.data.frame(read.csv("salaryNYC.csv"))
# 2013-2014
thirteen$pctNRA = thirteen$Nonresident.alien.total..EF2013A_RV..All.students..Undergraduate.total./thirteen$Grand.total..EF2013A_RV..All.students..Undergraduate.total.

ggplot(data=thirteen, aes(x=thirteen$Institution.Name, y=thirteen$pctNRA)) + geom_bar(stat="identity") + labs(x = "University", y = "Number of NRA's") + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1))+ggtitle("Number of International Students 2013-2014")

thirteen$minority2013 = (thirteen$Black.or.African.American.total...EF2013A_RV..All.students..Undergraduate.total.+thirteen$Hispanic.total...EF2013A_RV..All.students..Undergraduate.total.+thirteen$American.Indian.or.Alaska.Native.total...EF2013A_RV..All.students..Undergraduate.total.)/thirteen$Grand.total..EF2013A_RV..All.students..Undergraduate.total.

ggplot(data=thirteen, aes(x=thirteen$Institution.Name, y=thirteen$minority2013)) + geom_bar(stat="identity") + labs(x = "University", y = "Num. Blacks/Hispanics/Native Americans") + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + ggtitle("Minority Population 2013-2014")
 
plot(x=thirteen$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314., y = thirteen$minority2013, main = "Minority Pop. vs. Pell Grant 2013-2014", xlab = "Pell Grant %", ylab = "Minority Pop", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
#text(thirteen$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314.,(thirteen$Black.or.African.American.total...EF2013A_RV..All.students..Undergraduate.total.+thirteen$Hispanic.total...EF2013A_RV..All.students..Undergraduate.total.),labels = thirteen$Institution.Name, cex= 0.5, pos=1)

ggplot(data=thirteen, aes(x=thirteen$Institution.Name, y=thirteen$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314.)) + 
  geom_bar(stat="identity") + labs(x = "University", y = "Avg. % of Pell Grant Students") + theme(axis.text.x = element_text(size = 5, angle = 60, hjust = 1)) + ggtitle("Avg. % of Pell Grants Undergrad 2013-2014") + theme(axis.title.y = element_text(size = 7, angle = 90, vjust = -1)) + theme(plot.title = element_text(size = 10))

# Print top 5 over years
nra13 = thirteen[order(thirteen$pctNRA, decreasing = T),]
as.data.frame(head(nra13$Institution.Name, 10))

minority13 = thirteen[order(thirteen$minority2013, decreasing = T),]
as.data.frame(head(minority13$Institution.Name, 10))

pctpg13 = thirteen[order(thirteen$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314., decreasing = T),]
as.data.frame(head(pctpg13$Institution.Name, 10))

#2012-2013
twelve$pctNRA = twelve$Nonresident.alien.total..EF2012A_RV..All.students..Undergraduate.total./twelve$Grand.total..EF2012A_RV..All.students..Undergraduate.total.

ggplot(data=twelve, aes(x=twelve$Institution.Name, y=twelve$pctNRA)) +
  geom_bar(stat="identity") + labs(x = "University", y = "Number of NRA's") + theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1))+ggtitle("Number of International Students 2012-2013")

twelve$minority2012 = (twelve$Black.or.African.American.total...EF2012A_RV..All.students..Undergraduate.total.+twelve$Hispanic.total...EF2012A_RV..All.students..Undergraduate.total.+twelve$American.Indian.or.Alaska.Native.total...EF2012A_RV..All.students..Undergraduate.total.)/twelve$Grand.total..EF2012A_RV..All.students..Undergraduate.total.

ggplot(data=twelve, aes(x=twelve$Institution.Name, y=twelve$minority2012)) + geom_bar(stat="identity") + labs(x = "University", y = "Num. Blacks/Hispanics/Native Americans") + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + ggtitle("Minority Population 2012-2013")

plot(x=twelve$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV., y = twelve$minority2012, main = "Minority Pop. vs. Pell Grant 2012-2013", xlab = "Pell Grant %", ylab = "Minority Pop", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
#text(twelve$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV.,minority2012,labels = twelve$Institution.Name, cex= 0.5, pos=1)

ggplot(data=twelve, aes(x=twelve$Institution.Name, y=twelve$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV.)) + 
  geom_bar(stat="identity") + labs(x = "University", y = "Avg. % of Pell Grant Students") + theme(axis.text.x = element_text(size = 5, angle = 60, hjust = 1)) + ggtitle("Avg. % of Pell Grants Undergrad 2012-2013") + theme(axis.title.y = element_text(size = 7, angle = 90, vjust = -1)) + theme(plot.title = element_text(size = 10))

# Print top 5 over years
nra12 = twelve[order(twelve$pctNRA, decreasing = T),]
as.data.frame(head(nra12$Institution.Name, 10))

minority12 = twelve[order(twelve$minority2012, decreasing = T),]
as.data.frame(head(minority12$Institution.Name, 10))

pctpg12 = twelve[order(twelve$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV., decreasing = T),]
as.data.frame(head(pctpg12$Institution.Name, 10))

#2011-2012
eleven$pctNRA = eleven$Nonresident.alien.total..EF2011A_RV..All.students..Undergraduate.total./eleven$Grand.total..EF2011A_RV..All.students..Undergraduate.total.

ggplot(data=eleven, aes(x=eleven$Institution.Name, y=eleven$pctNRA)) +
  geom_bar(stat="identity") + labs(x = "University", y = "Number of NRA's") + theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1))+ggtitle("Number of International Students NYC2011-2012")

eleven$minority2011 = (eleven$Black.or.African.American.total...EF2011A_RV..All.students..Undergraduate.total.+eleven$Hispanic.total...EF2011A_RV..All.students..Undergraduate.total.+eleven$American.Indian.or.Alaska.Native.total...EF2011A_RV..All.students..Undergraduate.total.)/eleven$Grand.total..EF2011A_RV..All.students..Undergraduate.total.

ggplot(data=eleven, aes(x=eleven$Institution.Name, y=eleven$minority2011)) + geom_bar(stat="identity") + labs(x = "University", y = "Num. Blacks/Hispanics/Native Americans") + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + ggtitle("Minority Population 2011-2012")

plot(x=eleven$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV., y = eleven$minority2011, main = "Minority Pop. vs. Pell Grant 2011-2012", xlab = "Pell Grant %", ylab = "Minority Pop", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
#text(eleven$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112.,(eleven$Black.or.African.American.total...EF2011A_RV..All.students..Undergraduate.total.+eleven$Hispanic.total...EF2011A_RV..All.students..Undergraduate.total.),labels = eleven$Institution.Name, cex= 0.5, pos=1)

ggplot(data=eleven, aes(x=eleven$Institution.Name, y=eleven$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV.)) + 
  geom_bar(stat="identity") + labs(x = "University", y = "Avg. % of Pell Grant Students") + theme(axis.text.x = element_text(size = 5, angle = 60, hjust = 1)) + ggtitle("Avg. % of Pell Grants Undergrad 2011-2012") + theme(axis.title.y = element_text(size = 7, angle = 90, vjust = -1)) + theme(plot.title = element_text(size = 10))

# Print top 5 over years
nra11 = eleven[order(eleven$pctNRA, decreasing = T),]
as.data.frame(head(nra11$Institution.Name, 10))

minority11 = eleven[order(eleven$minority2011, decreasing = T),]
as.data.frame(head(minority11$Institution.Name, 10))

pctpg11 = eleven[order(eleven$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV., decreasing = T),]
as.data.frame(head(pctpg11$Institution.Name, 10))

######################################################### Tri State Area #########################################################
NYC = c(9,10,11,12,13,14,15,16,20,21,25,31,32,35,42,43,44,46,47,48,51,61)
NYCnames = eleven$Institution.Name[NYC]
elevenNYC = eleven[NYC,]
twelveNYC = twelve[NYC,]
thirteenNYC = thirteen[NYC,]

#2013-2014
thirteenNYC$pctNRA = thirteenNYC$Nonresident.alien.total..EF2013A_RV..All.students..Undergraduate.total./thirteenNYC$Grand.total..EF2013A_RV..All.students..Undergraduate.total.

ggplot(data=thirteenNYC, aes(x=thirteenNYC$Institution.Name, y=thirteenNYC$pctNRA)) +
  geom_bar(stat="identity") + labs(x = "University", y = "Number of NRA's") + theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1))+ ggtitle("Number of International Students 2013-2014 (NYC)")

thirteenNYC$minorityNYC = (thirteenNYC$Black.or.African.American.total...EF2013A_RV..All.students..Undergraduate.total.+thirteenNYC$Hispanic.total...EF2013A_RV..All.students..Undergraduate.total.+thirteenNYC$American.Indian.or.Alaska.Native.total...EF2013A_RV..All.students..Undergraduate.total.)/thirteenNYC$Grand.total..EF2013A_RV..All.students..Undergraduate.total.

ggplot(data=thirteenNYC, aes(x=thirteenNYC$Institution.Name, y=thirteenNYC$minorityNYC)) + geom_bar(stat="identity") + labs(x = "University", y = "Num. Blacks/Hispanics/Native Americans") + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + ggtitle("Minority Pop. 2013-2014 (NYC)")

ggplot(data=thirteenNYC, aes(x=thirteenNYC$Institution.Name, y=thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314.)) + 
  geom_bar(stat="identity") + labs(x = "University", y = "Avg. % of Pell Grant Students") + theme(axis.text.x = element_text(size = 5, angle = 60, hjust = 1)) + ggtitle("Avg. % of Pell Grants Undergrad 2013-2014 (NYC)") + theme(axis.title.y = element_text(size = 7, angle = 90, vjust = -1)) + theme(plot.title = element_text(size = 10))

plot(x=thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314., y = thirteenNYC$minorityNYC, main = "Minority Pop. vs. Pell Grant 2013-2014 (NYC)", xlab = "Pell Grant %", ylab = "Minority Pop", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
text(thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314.,thirteenNYC$minorityNYC,labels = thirteenNYC$Institution.Name, cex= 0.4, pos=1)
abline(glm(thirteenNYC$minorityNYC ~ thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314.))

NRAnyc13 = thirteenNYC[order(thirteenNYC$pctNRA, decreasing = T),]
as.data.frame(head(NRAnyc13$Institution.Name))

minority13nyc = thirteenNYC[order(thirteenNYC$minorityNYC, decreasing = T),]
as.data.frame(head(minority13nyc$Institution.Name))

pctpg13nyc = thirteenNYC[order(thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314., decreasing = T),]
as.data.frame(head(pctpg13nyc$Institution.Name))

#2012-2013

twelveNYC$pctNRA = twelveNYC$Nonresident.alien.total..EF2012A_RV..All.students..Undergraduate.total./twelveNYC$Grand.total..EF2012A_RV..All.students..Undergraduate.total.

ggplot(data=twelveNYC, aes(x=twelveNYC$Institution.Name, y=twelveNYC$pctNRA)) +
  geom_bar(stat="identity") + labs(x = "University", y = "Number of NRA's") + theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1))+ ggtitle("Number of International Students 2012-2013 (NYC)")

twelveNYC$minorityNYC = (twelveNYC$Black.or.African.American.total...EF2012A_RV..All.students..Undergraduate.total.+twelveNYC$Hispanic.total...EF2012A_RV..All.students..Undergraduate.total.+twelveNYC$American.Indian.or.Alaska.Native.total...EF2012A_RV..All.students..Undergraduate.total.)/twelveNYC$Grand.total..EF2012A_RV..All.students..Undergraduate.total.

ggplot(data=twelveNYC, aes(x=twelveNYC$Institution.Name, y=twelveNYC$minorityNYC)) + geom_bar(stat="identity") + labs(x = "University", y = "Num. Blacks/Hispanics/Native Americans") + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + ggtitle("Minority Pop. 2012-2013 (NYC)")

ggplot(data=twelveNYC, aes(x=twelveNYC$Institution.Name, y=twelveNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV.)) + 
  geom_bar(stat="identity") + labs(x = "University", y = "Avg. % of Pell Grant Students") + theme(axis.text.x = element_text(size = 5, angle = 60, hjust = 1)) + ggtitle("Avg. % of Pell Grants Undergrad 2012-2013 (NYC)") + theme(axis.title.y = element_text(size = 7, angle = 90, vjust = -1)) + theme(plot.title = element_text(size = 10))

plot(x=twelveNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV., y = twelveNYC$minorityNYC, main = "Minority Pop. vs. Pell Grant 2012-2013 (NYC)", xlab = "Pell Grant %", ylab = "Minority Pop", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
text(twelveNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV.,twelveNYC$minorityNYC,labels = twelveNYC$Institution.Name, cex= 0.4, pos=1)
abline(glm(twelveNYC$minorityNYC ~ twelveNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV.))

NRAnyc12 = twelveNYC[order(twelveNYC$pctNRA, decreasing = T),]
as.data.frame(head(NRAnyc12$Institution.Name))

minority12nyc = twelveNYC[order(twelveNYC$minorityNYC, decreasing = T),]
as.data.frame(head(minority12nyc$Institution.Name))

pctpg12nyc = twelveNYC[order(twelveNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1213_RV., decreasing = T),]
as.data.frame(head(pctpg12nyc$Institution.Name))

#2011-2012

elevenNYC$pctNRA = elevenNYC$Nonresident.alien.total..EF2011A_RV..All.students..Undergraduate.total./elevenNYC$Grand.total..EF2011A_RV..All.students..Undergraduate.total.

ggplot(data=elevenNYC, aes(x=elevenNYC$Institution.Name, y=elevenNYC$pctNRA)) +
  geom_bar(stat="identity") + labs(x = "University", y = "Number of NRA's") + theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1))+ ggtitle("Number of International Students 2011-2012 (NYC)")

elevenNYC$minorityNYC = (elevenNYC$Black.or.African.American.total...EF2011A_RV..All.students..Undergraduate.total.+elevenNYC$Hispanic.total...EF2011A_RV..All.students..Undergraduate.total.+elevenNYC$American.Indian.or.Alaska.Native.total...EF2011A_RV..All.students..Undergraduate.total.)/elevenNYC$Grand.total..EF2011A_RV..All.students..Undergraduate.total.

ggplot(data=elevenNYC, aes(x=elevenNYC$Institution.Name, y=elevenNYC$minorityNYC)) + geom_bar(stat="identity") + labs(x = "University", y = "Num. Blacks/Hispanics/Native Americans") + 
  theme(axis.text.x = element_text(size = 7, angle = 90, hjust = 1)) + ggtitle("Minority Pop. 2011-2012 (NYC)")

ggplot(data=elevenNYC, aes(x=elevenNYC$Institution.Name, y=elevenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV.)) + 
  geom_bar(stat="identity") + labs(x = "University", y = "Avg. % of Pell Grant Students") + theme(axis.text.x = element_text(size = 5, angle = 60, hjust = 1)) + ggtitle("Avg. % of Pell Grants Undergrad 2011-2012 (NYC)") + theme(axis.title.y = element_text(size = 7, angle = 90, vjust = -1)) + theme(plot.title = element_text(size = 10))

plot(x=elevenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV., y = elevenNYC$minorityNYC, main = "Minority Pop. vs. Pell Grant 2011-2012 (NYC)", xlab = "Pell Grant %", ylab = "Minority Pop", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
text(elevenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV.,elevenNYC$minorityNYC,labels = elevenNYC$Institution.Name, cex= 0.4, pos=1)
abline(glm(elevenNYC$minorityNYC ~ elevenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV.))

NRAnyc11 = elevenNYC[order(elevenNYC$pctNRA, decreasing = T),]
as.data.frame(head(NRAnyc11$Institution.Name))

minority11nyc = elevenNYC[order(elevenNYC$minorityNYC, decreasing = T),]
as.data.frame(head(minority11nyc$Institution.Name))

pctpg11nyc = elevenNYC[order(elevenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1112_RV., decreasing = T),]
as.data.frame(head(pctpg11nyc$Institution.Name))

################################################################################################################################################################
############################################################# Salary Comparison #############################################################
thirteenNYC = thirteenNYC[-(which(thirteenNYC$Institution.Name == "School of Visual Arts")),]
thirteenNYC = thirteenNYC[-(which(thirteenNYC$Institution.Name == "Vassar College")),]

#THIS STUFF DOESN'T WORK

#plot(x=salaryNYC$Institution.Name, y = salaryNYC$Mid.Career.Pay, main = "Mid-Career Pay (NYC)", xlab = "University", ylab = "Mid-Career Pay", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
#smexy = data.frame()
#smexy = salaryNYC$Institution.Name

# Mid-Career pay vs. Percent PG Students
#plot(x=thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314., y = salaryNYC$Mid.Career.Pay, main = "Mid-Career Pay vs. Pell Grant (NYC)", xlab = "Pell Grant %", ylab = "Mid-Career Pay", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
#text(thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314.,salaryNYC$Mid.Career.Pay,labels = salaryNYC$Mid.Career.Pay, cex= 0.4, pos=1)
#abline(glm(salaryNYC$Mid.Career.Pay ~ thirteenNYC$Percent.of.undergraduate.students.awarded.Pell.grants..SFA1314.))

#Mid-Career Pay vs. Minority Pop.
#plot(x=salaryNYC$Institution.Name, y = salaryNYC$Mid.Career.Pay, main = "Mid-Career Pay vs. Minority Pop. (NYC)", xlab = "Institution Name", ylab = "Mid-Career Pay", col= "blue", pch = 1, cex = .4, lty = "solid", lwd = .5)
#text(thirteenNYC$minorityNYC,salaryNYC$Mid.Career.Pay,labels = salaryNYC$Mid.Career.Pay, cex= 0.4, pos=1)
#abline(glm(salaryNYC$Mid.Career.Pay ~ thirteenNYC$minorityNYC))
#salaryNYC$Institution.Name
#typeof(salaryNYC$Institution.Name)
