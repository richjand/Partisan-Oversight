library(dplyr)
library(reshape2)
library(ggplot2)
library(tm)
library(lda)
library(topicmodels)
setwd("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/Data")
w = read.csv('plumwithwits.csv')
w$hearingid = as.character(w$hearingid)
w$mainagency  = as.character(w$mainagency)
w$appt = as.character(w$appt)
w$mainagency = ifelse(w$mainagency=='',NA,w$mainagency)
md = read.csv("C:/Users/richjand/Dropbox/Diss stuff/uniquehearingmetadata.csv")
names(md)[1] <- 'id'

wnomember = w[-(grep('a representative in congress', w$fulldescription ,ignore.case = T)),]

present = read.csv('ospresentmerged.csv')
present$hearingid = as.character(present$hearingid)
present$divgov = ifelse(present$congress %in% c(104,105,106,110,112),1,0)
present$housecontrol <- ifelse(present$congress %in% c(104,105,106,107,108,109,112),'R','D')

fullcommid = unique(present$hearingid[as.character(present$subcommittee)%in%c('fullcomm',' fullcomm')])

#DENSITIES OF IDEOLOGY FOR ATTENDING MEMBERS, REPUBLICANS IN DIVIDED V. UNIFIED, ALL HEARINGS#
pdf("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/repattend.pdf")
plot(density(present$nom1d[present$party==200 & present$housecontrol == 'R' & present$divgov==0], 
             na.rm = T, bw = .1), col = 'gray70', xlim = c(-.25,2), ylim = c(0,3),lty = 1,lwd = 2,
            xlab = 'DW-Nominate Score', main = "Republicans")
lines(density(present$nom1d[present$party==200 & present$housecontrol == 'R' & present$divgov==1], na.rm = T, bw = .1), col = 'black', lty = 1,lwd = 2)
lines(density(present$nom1d[present$party==200 & present$housecontrol == 'D' & present$divgov==1], na.rm = T, bw = .1), col = 'black', lty = 4,lwd = 2)
lines(density(present$nom1d[present$party==200 & present$housecontrol == 'D' & present$divgov==0], na.rm = T, bw = .1), col = 'gray70', lty = 4,lwd = 2)
legend(1.05,2.65, cex = .9,col = c('gray70','black','gray70','black'), lty = c(1,1,4,4),lwd = 2, c('Majority, Unified','Majority, Divided','Minority, Unified','Minority, Divided'))
dev.off()

#DENSITIES OF IDEOLOGY FOR ATTENDING MEMBERS, DEMOCRATS IN DIVIDED V. UNIFIED#
pdf("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/demattend.pdf")
plot(density(present$nom1d[present$party==100 & present$housecontrol == 'R' & present$divgov==0], 
             na.rm = T, bw = .1), col = 'gray70',lty = 1, ylim = c(0,3), lwd = 2,
     xlab = 'DW-Nominate Score', main = "Democrats")
lines(density(present$nom1d[present$party==100 & present$housecontrol == 'R' & present$divgov==1], na.rm = T, bw = .1), col = 'black', lty = 1,lwd = 2)
lines(density(present$nom1d[present$party==100 & present$housecontrol == 'D' & present$divgov==1], na.rm = T, bw = .1), col = 'black', lty = 4,lwd = 2)
lines(density(present$nom1d[present$party==100 & present$housecontrol == 'D' & present$divgov==0], na.rm = T, bw = .1), col = 'gray70', lty = 4,lwd = 2)
legend(-.25,2.65, cex = .9,col = c('gray70','black','gray70','black'), lty = c(1,1,4,4), c('Majority, Unified','Majority, Divided','Minority, Unified','Majority, Divided'),lwd = 2)
dev.off()


##SAME AS ABOVE, SUBCOMMITTEES ONLY##
plot(density(present$nom1d[present$party==200 & present$housecontrol == 'R' & present$divgov==0 & present$hearingid %in% fullcommid], 
             na.rm = T, bw = .1), col = 'firebrick1', xlim = c(-.25,2), ylim = c(0,3),lty = 1,lwd = 2,
     xlab = 'DW-Nominate Score', main = "Republicans")
lines(density(present$nom1d[present$party==200 & present$housecontrol == 'R' & present$divgov==1& present$hearingid %in% fullcommid], na.rm = T, bw = .1), col = 'black', lty = 1,lwd = 2)
lines(density(present$nom1d[present$party==200 & present$housecontrol == 'D' & present$divgov==1& present$hearingid %in% fullcommid], na.rm = T, bw = .1), col = 'black', lty = 4,lwd = 2)
lines(density(present$nom1d[present$party==200 & present$housecontrol == 'D' & present$divgov==0& present$hearingid %in% fullcommid], na.rm = T, bw = .1), col = 'gray70', lty = 4,lwd = 2)
legend(1.05,2.65, cex = .9,col = c('red','black','red','black'), lty = c(1,1,4,4), c('Rep House, Unified','Rep House, Divided','Dem House, Unified','Dem House, Divided'),lwd = 2)


plot(density(present$nom1d[present$party==100 & present$housecontrol == 'R' & present$divgov==0& present$hearingid %in% fullcommid], 
             na.rm = T, bw = .1), col = 'firebrick1',lty = 1, ylim = c(0,3),
     xlab = 'DW-Nominate Score', main = "Democrats")
lines(density(present$nom1d[present$party==100 & present$housecontrol == 'R' & present$divgov==1& present$hearingid %in% fullcommid], na.rm = T, bw = .1), col = 'black', lty = 1)
lines(density(present$nom1d[present$party==100 & present$housecontrol == 'D' & present$divgov==1& present$hearingid %in% fullcommid], na.rm = T, bw = .1), col = 'black', lty = 4)
lines(density(present$nom1d[present$party==100 & present$housecontrol == 'D' & present$divgov==0& present$hearingid %in% fullcommid], na.rm = T, bw = .1), col = 'firebrick1', lty = 4)
legend(-.25,2.65, cex = .9,col = c('red','black','red','black'), lty = c(1,1,4,4), c('Rep House, Unified','Rep House, Divided','Dem House, Unified','Dem House, Divided'))



mostcommon <- tail(sort(table(wnomember$mainagency)),10)
mc <- data.frame(count = mostcommon, agency = names(mostcommon))
mc$agency = factor(mc$agency, levels = mc[order(mc$count),'agency'])
mcplot <- ggplot(mc, aes(x = count, y = agency)) + 
  geom_point() + 
  scale_y_discrete(breaks=rev(mc$agency), labels=c("GAO", "Defense", "HHS", "Executive Office of the President",
                                                      "Justice","Homeland Security","State","Treasury", "Commerce","Office of Personnel Management")) + 
  xlab("Number of Witnesses") +
  ylab('') +
  xlim(c(0,700))
ggsave("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/witnesscountbyagency.pdf")

id = unique(w$hearingid)
bureaucrat = 0
eop = 0
majpres = 0
minpres = 0
totalpres = 0
executivebranchwit = 0
ag = 0
commerce = 0
defense = 0
education = 0
energy = 0
hhs = 0
hud = 0
interior = 0
justice = 0
labor = 0
state = 0
transportation = 0
treasury = 0
va = 0
pa = 0
pas = 0
for (x in 1:length(id)){
  sub = w[w$hearingid == id[x],]
  bureaucrat[x] = ifelse(sum(is.na(sub$mainagency))==length(sub$mainagency), 0, 1)
  eop[x] = ifelse('executive office of the president' %in% sub$mainagency, 1, 0)
  ag[x] = ifelse('department of agriculture' %in% sub$mainagency, 1, 0)
  commerce[x] = ifelse('department of commerce' %in% sub$mainagency, 1, 0)
  defense[x] = ifelse('office of the secretary of defense' %in% sub$mainagency, 1, 0)
  education[x] = ifelse('department of education' %in% sub$mainagency, 1, 0)
  energy[x] = ifelse('department of energy' %in% sub$mainagency, 1, 0)
  hhs[x] = ifelse('department of health and human services' %in% sub$mainagency, 1, 0)
  hud[x] = ifelse('department of housing and urban development' %in% sub$mainagency, 1, 0)
  interior[x] = ifelse('department of the interior' %in% sub$mainagency, 1, 0)
  justice[x] = ifelse('department of justice' %in% sub$mainagency, 1, 0)
  labor[x] = ifelse('department of labor' %in% sub$mainagency, 1, 0)
  state[x] = ifelse('department of state' %in% sub$mainagency, 1, 0)
  transportation[x] = ifelse('department of transportation' %in% sub$mainagency, 1, 0)
  treasury[x] = ifelse('department of the treasury' %in% sub$mainagency, 1, 0)
  va[x] = ifelse('department of veterans affairs' %in% sub$mainagency, 1, 0)
  psub = present[present$hearingid == id[x],]
  majpres[x] = nrow(psub[psub$Maj.Min==1,])
  minpres[x] = nrow(psub[psub$Maj.Min==2,])
  totalpres[x] = majpres[x] + minpres[x]
  executivebranchwit[x] = ifelse(1 %in% sub$executivebranch,1,0)
  pa[x] = ifelse('PA' %in% sub$appt,1,0)
  pas[x] = ifelse('PAS' %in% sub$appt,1,0)
}

hearings = data.frame(id,bureaucrat,eop,majpres,minpres,totalpres,executivebranchwit,pa,pas,
                      ag,commerce,defense,education,energy,hhs,hud,interior,
                      justice,labor,state,transportation,treasury,va)
hearings$congress = as.numeric(substr(hearings$id,6,8))
hearings$divgov = ifelse(hearings$congress %in% c(104,105,106,110,112),1,0)
hearings$majmindiff = hearings$majpres - hearings$minpres
hearings$majoritypercent = hearings$majpres/hearings$totalpres

hearings$president = NA 
for (i in 1:nrow(hearings)){
  if (hearings$congress[i] %in% 104:106) hearings$president[i] = "Clinton"
  if (hearings$congress[i] %in% 107:110) hearings$president[i] = "Bush"
  if (hearings$congress[i] %in% 111:112) hearings$president[i] = "Obama"
}

hearings$housecontrol <- ifelse(hearings$congress %in% c(104,105,106,107,108,109,112),'R','D')
hearings <- merge(hearings, md[,c(1,3)], by = 'id', all.x = T)
hearings$year = as.numeric(substr(hearings$date,1,5))
hearings$preselection <- ifelse(hearings$year %in% c(1996, 2000, 2004, 2008, 2012),1,0)

split = group_by(hearings, divgov)
agencydiffs = summarise(split,
                        Agriculture = mean(ag),
                        Commerce = mean(commerce),
                        Defense = mean(defense),
                        Education = mean(education),
                        EOP = mean(eop),
                        Energy = mean(energy),
                        HHS = mean(hhs),
                        HUD = mean(hud),
                        Interior = mean(interior),
                        Justice = mean(justice),
                        Labor = mean(labor),
                        State = mean(state),
                        Transportation = mean(transportation),
                        Treasury = mean(treasury),
                        VA = mean(va))
agencydiffs[3,] <- (agencydiffs[2,]-agencydiffs[1,])/agencydiffs[1,]
rownames(agencydiffs) = c("Unified",'Divided','Percent.Change')
agencydiffs <- t(agencydiffs)[-1,]
agencydiffs <- as.data.frame(apply(agencydiffs,2,as.numeric))
rownames(agencydiffs) = c("Agriculture",'Commerce','Defense','Education','EOP',
                         'Energy','HHS','HUD','Interior','Justice','Labor','State',
                         'Transportation','Treasury','VA')
agencydiffs$Percent.Change <- agencydiffs$Percent.Change * 100
agencydiffs$Agency = rownames(agencydiffs)
agencydiffs$Agency = factor(agencydiffs$Agency, levels = agencydiffs[order(agencydiffs$Percent.Change),'Agency'])
agencydiffs = agencydiffs[order(agencydiffs$Percent.Change),]
probchange <- ggplot(agencydiffs, aes(x = Percent.Change, y = Agency)) + geom_point()
probchange + ggtitle("") + xlim(c(-60,60)) + geom_vline(xintercept = 0, linetype = "longdash") + xlab("Percent change ")
ggsave("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/agencychanges.pdf")

##REGRESSIONS BY COMMITTEE##
coef = NA
se = NA
comms = c('ag','commerce','defense','education','energy','hhs','hud','interior','justice','labor','transportation','va', 'state', 'treasury')
for (z in 1:length(comms)){
  com = comms[z]
  dv = hearings[,com]
  reg = summary(glm(dv~hearings$divgov + hearings$president, family = binomial))
  coefs = reg$coef
  coef[z] = coefs[2,1]
  se[z] = coefs[2,2]
}

bycomm = data.frame(coef,se,comms)
bycomm$comms = reorder(bycomm$comms, -bycomm$coef)
x = ggplot(data = bycomm, aes(y = coef, x = comms)) + geom_point(size = 3, colour = 'red') + geom_pointrange(aes(ymin = coef - 1.96*se, ymax = coef + 1.96*se)) + ylim(c(-4.5,2)) + geom_hline(aes(yintercept = 0)) +
 ylab("Coefficient") + xlab("Agency") + theme(text = element_text(size=18), axis.text.x = element_text( hjust = .5, vjust = 0)) +
  scale_x_discrete(breaks=c('ag','labor','va','hhs','commerce','interior','treasury','defense','energy','state','transportation','justice','hud','education'), 
  labels=c('Agriculture','Labor','VA','HHS','Commerce','Interior','Treasury','Defense','Energy','State','Transportation','Justice','HUD','Education')) + coord_flip()
ggsave("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/coefbyagency.pdf")

hearings = hearings[hearings$totalpres!=0,]
#Probability of Executive Branch witness#
log.1 <- glm(bureaucrat~divgov, family = binomial, data = hearings)
log.2 <- glm(eop~divgov, family = binomial, data = hearings)
log.3 <- glm(executivebranchwit~divgov, family = binomial, data = hearings)
log.4 <- glm(pas~divgov, family = binomial, data = hearings)

#With President fixed effects#
log.5 <- glm(bureaucrat~divgov + president + housecontrol, family = binomial, data = hearings)
log.6 <- glm(eop~divgov + president + housecontrol, family = binomial, data = hearings)
log.7 <- glm(executivebranchwit~divgov + president + housecontrol, family = binomial, data = hearings)
log.8 <- glm(pas~divgov + president + housecontrol, family = binomial, data = hearings)

#Counts for majority and minority#
log.9 <- glm(majpres~divgov + president, family = "poisson", data = hearings)
log.10 <- glm(minpres~divgov + president, family = "poisson", data = hearings)
log.11 <- summary(glm(majmindiff~divgov + president, data = hearings))
log.12 <- summary(glm(majoritypercent~divgov + president, family = binomial, data = hearings))

##PLOT OF LOGIT COEFS##
plotdata <- data.frame(
  dvs = c('Bureaucrat','Executive Office of the President', 'Executive Branch','Senate-Confirmed Appointee'),
  coefs = c(summary(log.5)$coef[2,1],summary(log.6)$coef[2,1] ,summary(log.7)$coef[2,1] ,summary(log.8)$coef[2,1] ),
  se = c(summary(log.5)$coef[2,2],summary(log.6)$coef[2,2] ,summary(log.7)$coef[2,2] ,summary(log.8)$coef[2,2] )
  
  )

coefplot <- ggplot(plotdata,aes(y = dvs, x = coefs)) + geom_point(size = 4, colour = 'red') + geom_errorbarh(aes(xmin = coefs-1.96*se, xmax = coefs + 1.96*se), height = .1)
coefplot + xlim(c(-1.5,1.5)) + xlab("Effect of Divided Government") + ylab("") + geom_vline(xintercept = 0, linetype = 2)
ggsave("C:/Users/richjand/Dropbox/Diss stuff/partisan oversight/coefplot.pdf")
##BUILDING CORPUS OF HEARING TITLES##
id = hearings$id
title <- character()
for (z in 1:length(id)){
  text <- readLines(paste('E:/Hearings/GPO/',id[z], '.htm', sep = ''))
  title[z] <- gsub('[[:punct:]]','', text[2])
  title[z] <- gsub('title','',title[z]) 
}

mydata.corpus <- Corpus(VectorSource(title))
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
mydata.corpus <- tm_map(mydata.corpus, removeNumbers)
mydata.corpus <- tm_map(mydata.corpus, tolower)
mydata.corpus <- tm_map(mydata.corpus, stemDocument)
mydata.corpus <- tm_map(mydata.corpus, removeWords, stopwords('english'))
mydata.dtm <- TermDocumentMatrix(mydata.corpus)

z = inspect(t(mydata.dtm))
z = data.frame(id,hearings$divgov,z)

bush = z[grepl('(109h|110h)',z$id),]
bush = bush[,-1]
bg <- group_by(bush,hearings.divgov)
bsums <- summarise_each(bg,funs(sum))
bsums[3,] <- bsums[2,] - bsums[1,]
bsums <- t(bsums[,-1])

bushdiffs <- data.frame(bu = bsums[,1], bdiv = bsums[,2], bdiff = bsums[,3])
bushdiffs$abs <- abs(bushdiffs$bdiff)
bushdiffs <- bushdiffs[order(bushdiffs$abs),]
tail(bushdiffs,20)

obama = z[grepl('(111h|112h)',z$id),]
obama = obama[,-1]
og <- group_by(obama,hearings.divgov)
osums <- summarise_each(og,funs(sum))
osums[3,] <- osums[2,] - osums[1,]
osums <- t(osums[,-1])

obdiffs <- data.frame(ou = osums[,1], odiv = osums[,2], odiff = osums[,3])
obdiffs$abs <- abs(obdiffs$odiff)
obdiffs <- obdiffs[order(obdiffs$abs),]
tail(obdiffs,20)


model <- LDA(mydata.dtm,k = 10)
terms(model,5)
