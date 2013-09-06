require(plyr)
require(flightcallr)
require(randomForest)


# note, this function is not intended to be actually run... it's just a
# container for a bunch of code that I'll paste into the interpreter or put into
# the main program body
construct_bootstrap_tables = function() {
  load("~/random_forest/nyserda/allegany.rda")
  allegany.sparrow.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ALLEGANY_20120914_185000.Sp100.02_241882selections.1-5_DAC_aek.txt", Sound_File_Path=".")
  allegany.sparrow.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ALLEGANY_20120914_185000.Sp100.02_241882selections.6-10_DAC_aek.txt", Sound_File_Path=".")
  allegany.sparrow.11to15 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ALLEGANY_20120914_185000.Sp100.02_241882selections.11-15_DAC_aek.txt", Sound_File_Path=".")
  allegany.sparrow.bootstrap = rbind.fill(allegany.sparrow.1to5, allegany.sparrow.6to10, allegany.sparrow.11to15)
  allegany.sparrow = allegany[allegany$Random.Percent <= 15, ]
  allegany.sparrow = allegany.sparrow[order(allegany.sparrow$Selection), ]
  allegany.sparrow.bootstrap = allegany.sparrow.bootstrap[order(allegany.sparrow.bootstrap$Selection), ]
  allegany.sparrow$Call_vs_Noise = allegany.sparrow.bootstrap$Call_vs_Noise
  allegany.sparrow.bootstrap = allegany.sparrow


  load("~/random_forest/nyserda/danby.rda")
  danby.sparrow.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/DANBY-_20120914_184500.Sp100.02_9436selections.1-5_DAC_aek.txt", Sound_File_Path = ".")
  danby.sparrow.bootstrap = danby.sparrow.1to5
  danby.sparrow = danby[danby$Random.Percent <= 5, ]
  danby.sparrow = danby.sparrow[order(danby.sparrow$Selection), ]
  danby.sparrow.bootstrap = danby.sparrow.bootstrap[order(danby.sparrow.bootstrap$Selection), ]
  danby.sparrow$Call_vs_Noise = danby.sparrow.bootstrap$Call_vs_Noise
  danby.sparrow.bootstrap = danby.sparrow



  load("~/random_forest/nyserda/lewis.rda")
  lewis.sparrow.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/LEWIS_20120914_184300.Sp100.02_36126selections.1-5_DAC_aek.txt", Sound_File_Path=".")
  lewis.sparrow.bootstrap = lewis.sparrow.1to5
  lewis.sparrow = lewis[lewis$Random.Percent <= 5, ]
  lewis.sparrow = lewis.sparrow[order(lewis.sparrow$Selection), ]
  lewis.sparrow.bootstrap = lewis.sparrow.bootstrap[order(lewis.sparrow.bootstrap$Selection), ]
  lewis.sparrow$Call_vs_Noise = lewis.sparrow.bootstrap$Call_vs_Noise
  lewis.sparrow.bootstrap = lewis.sparrow



  load("~/random_forest/nyserda/madison.rda")
  madison.sparrow.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/MADISON_20120914_184200.Sp100.02_75849selections.1-5_DAC_aek.txt", Sound_File_Path = ".")
  madison.sparrow.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/MADISON_20120914_184200.Sp100.02_75849selections.6-10_DAC_aek.txt", Sound_File_Path = ".")
  madison.sparrow.bootstrap = rbind.fill(madison.sparrow.1to5, madison.sparrow.6to10)
  madison.sparrow = madison[madison$Random.Percent <= 10, ]
  madison.sparrow = madison.sparrow[order(madison.sparrow$Selection), ]
  madison.sparrow.bootstrap = madison.sparrow.bootstrap[order(madison.sparrow.bootstrap$Selection), ]
  madison.sparrow$Call_vs_Noise = madison.sparrow.bootstrap$Call_vs_Noise
  madison.sparrow.bootstrap = madison.sparrow



  load("~/random_forest/nyserda/ontario.rda")
  ontario.sparrow.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ONTARIO_20120914_184900.Sp100.02_48369selections.1-5_DAC_aek.txt", Sound_File_Path = ".")
  ontario.sparrow.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ONTARIO_20120914_184900.Sp100.02_48369selections.6-10_DAC_aek.txt", Sound_File_Path = ".")
  ontario.sparrow.bootstrap = rbind.fill(ontario.sparrow.1to5, ontario.sparrow.6to10)
  ontario.sparrow = ontario[ontario$Random.Percent <= 10, ]
  ontario.sparrow = ontario.sparrow[order(ontario.sparrow$Selection), ]
  ontario.sparrow.bootstrap = ontario.sparrow.bootstrap[order(ontario.sparrow.bootstrap$Selection), ]
  ontario.sparrow$Call_vs_Noise = ontario.sparrow.bootstrap$Call_vs_Noise
  ontario.sparrow.bootstrap = ontario.sparrow



  load("~/random_forest/nyserda/phr.rda")
  phr.sparrow.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/PHR_20120914_184600.Sp100.02_36755selections.1-5_DAC_aek.txt", Sound_File_Path = ".")
  phr.sparrow.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/PHR_20120914_184600.Sp100.02_36755selections.6-10_DAC_aek.txt", Sound_File_Path = ".")
  phr.sparrow.11to15 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/PHR_20120914_184600.Sp100.02_36755selections.11-15_DAC_aek.txt", Sound_File_Path = ".")
  phr.sparrow.16to20 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/PHR_20120914_184600.Sp100.02_36755selections.16-20_DAC_aek.txt", Sound_File_Path = ".")
  phr.sparrow.bootstrap = rbind.fill(phr.sparrow.1to5, phr.sparrow.6to10, phr.sparrow.11to15, phr.sparrow.16to20)
  phr.sparrow = phr[phr$Random.Percent <= 20, ]
  phr.sparrow = phr.sparrow[order(phr.sparrow$Selection), ]
  phr.sparrow.bootstrap = phr.sparrow.bootstrap[order(phr.sparrow.bootstrap$Selection), ]
  phr.sparrow$Call_vs_Noise = phr.sparrow.bootstrap$Call_vs_Noise
  phr.sparrow.bootstrap = phr.sparrow



  load("~/random_forest/nyserda/wyoming.rda")
  wyoming.sparrow.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/WYOMING_20120914_185300.Sp100.02_17353selections.1-5_DAC_aek.txt", Sound_File_Path=".")
  wyoming.sparrow.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/WYOMING_20120914_185300.Sp100.02_17353selections.6-10_DAC_aek.txt", Sound_File_Path=".")
  wyoming.sparrow.bootstrap = rbind.fill(wyoming.sparrow.1to5, wyoming.sparrow.6to10)
  wyoming.sparrow = wyoming[wyoming$Random.Percent <= 10, ]
  wyoming.sparrow = wyoming.sparrow[order(wyoming.sparrow$Selection), ]
  wyoming.sparrow.bootstrap = wyoming.sparrow.bootstrap[order(wyoming.sparrow.bootstrap$Selection), ]
  wyoming.sparrow$Call_vs_Noise = wyoming.sparrow.bootstrap$Call_vs_Noise
  wyoming.sparrow.bootstrap = wyoming.sparrow


  # variable from this will be called NSDNS.2012 despite name of rda file
  load("~/random_forest/NovaScotia/NSDNS.Spring2012.rda")
  NSDNS.2012.sparrow.bootstrap = NSDNS.2012[NSDNS.2012$Random.Percent <= 15, ]



  load("~/random_forest/NovaScotia/NSNME.2012.rda")
  NSNME.2012.sparrow.bootstrap = NSNME.2012[NSNME.2012$Random.Percent <= 20, ]



  load("~/random_forest/NovaScotia/NSGDN.2012.rda")
  NSGDN.2012.sel = read.selection.table("p:/2012_Nova Scotia/whole deployment selection tables/GDN_GDS_LVN_dropoff/NSGDN_20120423_20120615.Sp100.2_percentiles_1-15done.txt", Sound_File_Path = ".")
  NSGDN.2012.sel = NSGDN.2012.sel[NSGDN.2012.sel$Random.Percent <= 15, ]
  NSGDN.2012.sparrow.bootstrap = NSGDN.2012[NSGDN.2012$Random.Percent <= 15, ]
  NSGDN.2012.sparrow.bootstrap$Call_vs_Noise = NSGDN.2012.sel$Call_vs_Noise



  load("~/random_forest/NovaScotia/NSGDS.2012.rda")
  NSGDS.2012.sel = read.selection.table("p:/2012_Nova Scotia/whole deployment selection tables/GDN_GDS_LVN_dropoff/NSGDS_20120424_20120615.Sp100.2_percentiles_1-15done.txt", Sound_File_Path = ".")
  NSGDS.2012.sel = NSGDS.2012.sel[NSGDS.2012.sel$Random.Percent <= 15, ]
  NSGDS.2012.sparrow.bootstrap = NSGDS.2012[NSGDS.2012$Random.Percent <= 15, ]
  NSGDS.2012.sparrow.bootstrap$Call_vs_Noise = NSGDS.2012.sel$Call_vs_Noise



  load("~/random_forest/NovaScotia/NSLVN.2012.rda")
  NSLVN.2012.sel = read.selection.table("p:/2012_Nova Scotia/whole deployment selection tables/GDN_GDS_LVN_dropoff/NSLVN_20120428to20120615.Sp100.2_percentiles_1-15_aek.txt", Sound_File_Path=".")
  NSLVN.2012.sel = NSLVN.2012.sel[NSLVN.2012.sel$Random.Percent <= 15, ]
  NSLVN.2012.sparrow.bootstrap = NSLVN.2012[NSLVN.2012$Random.Percent <= 15, ]
  NSLVN.2012.sparrow.bootstrap$Call_vs_Noise = NSLVN.2012.sel$Call_vs_Noise



  load("~/random_forest/NovaScotia/NSDNS.Fall2011.rda")
  NSDNS.2011.sparrow.bootstrap = NSDNS.2011[NSDNS.2011$Random.Percent <= 5, ]



  load("~/random_forest/NovaScotia/NSNME.Fall2011.rda")
  NSNME.2011.sparrow.bootstrap = NSNME.2011[NSNME.2011$Random.Percent <= 10, ]



  load("~/random_forest/NovaScotia/NSGDN.2011.rda")
  NSGDN.2011.sparrow.bootstrap = NSGDN.2011[NSGDN.2011$Random.Percent <= 5, ]



  load("~/random_forest/NovaScotia/NSGDS.Fall2011.rda")
  NSGDS.2011.sparrow.bootstrap = NSGDS.2011[NSGDS.2011$Random.Percent <= 5, ]



  # FIX - wtf? that table is messed up in the dropoff folder
#  load("~/random_forest/NovaScotia/NSLVN.rda")
#  NSLVN.2011.sel = read.selection.table("p:/2011_NovaScotia/whole deployment selection tables/GDN_GDS_LVN_dropoff/NSLVN_20110908_20111101.Sp100.2_percentiles_1-5doneaek.txt", Sound_File_Path = ".")
#  NSLVN.2011.sel = NSLVN.2011.sel[NSLVN.2011.sel$Random.Percent <= 5, ]


}



allegany.sparrow.call = allegany.sparrow.bootstrap[allegany.sparrow.bootstrap$Call_vs_Noise == "Call", ]
allegany.sparrow.noise = allegany.sparrow.bootstrap[allegany.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(allegany.sparrow.call)
allegany.sparrow.noise = allegany.sparrow.noise[sample(nrow(allegany.sparrow.noise), 10 * num.calls), ]
allegany.sparrow.samp = rbind(allegany.sparrow.call, allegany.sparrow.noise)

danby.sparrow.call = danby.sparrow.bootstrap[danby.sparrow.bootstrap$Call_vs_Noise == "Call", ]
danby.sparrow.noise = danby.sparrow.bootstrap[danby.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(danby.sparrow.call)
danby.sparrow.noise = danby.sparrow.noise[sample(nrow(danby.sparrow.noise), 10 * num.calls), ]
danby.sparrow.samp = rbind(danby.sparrow.call, danby.sparrow.noise)

lewis.sparrow.call = lewis.sparrow.bootstrap[lewis.sparrow.bootstrap$Call_vs_Noise == "Call", ]
lewis.sparrow.noise = lewis.sparrow.bootstrap[lewis.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(lewis.sparrow.call)
lewis.sparrow.noise = lewis.sparrow.noise[sample(nrow(lewis.sparrow.noise), 10 * num.calls), ]
lewis.sparrow.samp = rbind(lewis.sparrow.call, lewis.sparrow.noise)

madison.sparrow.call = madison.sparrow.bootstrap[madison.sparrow.bootstrap$Call_vs_Noise == "Call", ]
madison.sparrow.noise = madison.sparrow.bootstrap[madison.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(madison.sparrow.call)
madison.sparrow.noise = madison.sparrow.noise[sample(nrow(madison.sparrow.noise), 10 * num.calls), ]
madison.sparrow.samp = rbind(madison.sparrow.call, madison.sparrow.noise)

NSDNS.2011.sparrow.call = NSDNS.2011.sparrow.bootstrap[NSDNS.2011.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSDNS.2011.sparrow.noise = NSDNS.2011.sparrow.bootstrap[NSDNS.2011.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSDNS.2011.sparrow.call)
NSDNS.2011.sparrow.noise = NSDNS.2011.sparrow.noise[sample(nrow(NSDNS.2011.sparrow.noise), 10 * num.calls), ]
NSDNS.2011.sparrow.samp = rbind(NSDNS.2011.sparrow.call, NSDNS.2011.sparrow.noise)

NSDNS.2012.sparrow.call = NSDNS.2012.sparrow.bootstrap[NSDNS.2012.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSDNS.2012.sparrow.noise = NSDNS.2012.sparrow.bootstrap[NSDNS.2012.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSDNS.2012.sparrow.call)
NSDNS.2012.sparrow.noise = NSDNS.2012.sparrow.noise[sample(nrow(NSDNS.2012.sparrow.noise), 10 * num.calls), ]
NSDNS.2012.sparrow.samp = rbind(NSDNS.2012.sparrow.call, NSDNS.2012.sparrow.noise)

NSGDN.2011.sparrow.call = NSGDN.2011.sparrow.bootstrap[NSGDN.2011.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSGDN.2011.sparrow.noise = NSGDN.2011.sparrow.bootstrap[NSGDN.2011.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSGDN.2011.sparrow.call)
NSGDN.2011.sparrow.noise = NSGDN.2011.sparrow.noise[sample(nrow(NSGDN.2011.sparrow.noise), 10 * num.calls), ]
NSGDN.2011.sparrow.samp = rbind(NSGDN.2011.sparrow.call, NSGDN.2011.sparrow.noise)

NSGDN.2012.sparrow.call = NSGDN.2012.sparrow.bootstrap[NSGDN.2012.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSGDN.2012.sparrow.noise = NSGDN.2012.sparrow.bootstrap[NSGDN.2012.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSGDN.2012.sparrow.call)
NSGDN.2012.sparrow.noise = NSGDN.2012.sparrow.noise[sample(nrow(NSGDN.2012.sparrow.noise), 10 * num.calls), ]
NSGDN.2012.sparrow.samp = rbind(NSGDN.2012.sparrow.call, NSGDN.2012.sparrow.noise)

NSGDS.2011.sparrow.call = NSGDS.2011.sparrow.bootstrap[NSGDS.2011.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSGDS.2011.sparrow.noise = NSGDS.2011.sparrow.bootstrap[NSGDS.2011.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSGDS.2011.sparrow.call)
NSGDS.2011.sparrow.noise = NSGDS.2011.sparrow.noise[sample(nrow(NSGDS.2011.sparrow.noise), 10 * num.calls), ]
NSGDS.2011.sparrow.samp = rbind(NSGDS.2011.sparrow.call, NSGDS.2011.sparrow.noise)

NSGDS.2012.sparrow.call = NSGDS.2012.sparrow.bootstrap[NSGDS.2012.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSGDS.2012.sparrow.noise = NSGDS.2012.sparrow.bootstrap[NSGDS.2012.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSGDS.2012.sparrow.call)
NSGDS.2012.sparrow.noise = NSGDS.2012.sparrow.noise[sample(nrow(NSGDS.2012.sparrow.noise), 10 * num.calls), ]
NSGDS.2012.sparrow.samp = rbind(NSGDS.2012.sparrow.call, NSGDS.2012.sparrow.noise)

NSNME.2011.sparrow.call = NSNME.2011.sparrow.bootstrap[NSNME.2011.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSNME.2011.sparrow.noise = NSNME.2011.sparrow.bootstrap[NSNME.2011.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSNME.2011.sparrow.call)
NSNME.2011.sparrow.noise = NSNME.2011.sparrow.noise[sample(nrow(NSNME.2011.sparrow.noise), 10 * num.calls), ]
NSNME.2011.sparrow.samp = rbind(NSNME.2011.sparrow.call, NSNME.2011.sparrow.noise)

NSNME.2012.sparrow.call = NSNME.2012.sparrow.bootstrap[NSNME.2012.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSNME.2012.sparrow.noise = NSNME.2012.sparrow.bootstrap[NSNME.2012.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSNME.2012.sparrow.call)
NSNME.2012.sparrow.noise = NSNME.2012.sparrow.noise[sample(nrow(NSNME.2012.sparrow.noise), 10 * num.calls), ]
NSNME.2012.sparrow.samp = rbind(NSNME.2012.sparrow.call, NSNME.2012.sparrow.noise)

NSLVN.2012.sparrow.call = NSLVN.2012.sparrow.bootstrap[NSLVN.2012.sparrow.bootstrap$Call_vs_Noise == "Call", ]
NSLVN.2012.sparrow.noise = NSLVN.2012.sparrow.bootstrap[NSLVN.2012.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(NSLVN.2012.sparrow.call)
NSLVN.2012.sparrow.noise = NSLVN.2012.sparrow.noise[sample(nrow(NSLVN.2012.sparrow.noise), 10 * num.calls), ]
NSLVN.2012.sparrow.samp = rbind(NSLVN.2012.sparrow.call, NSLVN.2012.sparrow.noise)

ontario.sparrow.call = ontario.sparrow.bootstrap[ontario.sparrow.bootstrap$Call_vs_Noise == "Call", ]
ontario.sparrow.noise = ontario.sparrow.bootstrap[ontario.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(ontario.sparrow.call)
ontario.sparrow.noise = ontario.sparrow.noise[sample(nrow(ontario.sparrow.noise), 10 * num.calls), ]
ontario.sparrow.samp = rbind(ontario.sparrow.call, ontario.sparrow.noise)

wyoming.sparrow.call = wyoming.sparrow.bootstrap[wyoming.sparrow.bootstrap$Call_vs_Noise == "Call", ]
wyoming.sparrow.noise = wyoming.sparrow.bootstrap[wyoming.sparrow.bootstrap$Call_vs_Noise == "Noise", ]
num.calls = nrow(wyoming.sparrow.call)
wyoming.sparrow.noise = wyoming.sparrow.noise[sample(nrow(wyoming.sparrow.noise), 10 * num.calls), ]
wyoming.sparrow.samp = rbind(wyoming.sparrow.call, wyoming.sparrow.noise)

rm(list = ls(pattern="(call|noise)$"))

#bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )



bootstrap.all = rbind.fill(danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out allegany")
print(my.rf)
allegany.pred = predict(my.rf, allegany.sparrow.samp, type="prob")
allegany.pred = data.frame(allegany.pred)
allegany.pred$Call_vs_Noise = allegany.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=allegany.pred, main="Combined Model Built Minus allegany:\nPredictions Against allegany", xlab="Class in allegany bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=allegany.pred, geom="violin", ylab="Model Score", xlab="Class in allegany bootstrap", main="Combined Model Built Minus allegany:\nPredictions Against allegany")

bootstrap.all = rbind.fill(allegany.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out danby")
print(my.rf)
danby.pred = predict(my.rf, danby.sparrow.samp, type="prob")
danby.pred = data.frame(danby.pred)
danby.pred$Call_vs_Noise = danby.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=danby.pred, main="Combined Model Built Minus danby:\nPredictions Against danby", xlab="Class in danby bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=danby.pred, geom="violin", ylab="Model Score", xlab="Class in danby bootstrap", main="Combined Model Built Minus danby:\nPredictions Against danby")

bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out lewis")
print(my.rf)
lewis.pred = predict(my.rf, lewis.sparrow.samp, type="prob")
lewis.pred = data.frame(lewis.pred)
lewis.pred$Call_vs_Noise = lewis.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=lewis.pred, main="Combined Model Built Minus lewis:\nPredictions Against lewis", xlab="Class in lewis bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=lewis.pred, geom="violin", ylab="Model Score", xlab="Class in lewis bootstrap", main="Combined Model Built Minus lewis:\nPredictions Against lewis")

bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out madison")
print(my.rf)
madison.pred = predict(my.rf, madison.sparrow.samp, type="prob")
madison.pred = data.frame(madison.pred)
madison.pred$Call_vs_Noise = madison.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=madison.pred, main="Combined Model Built Minus madison:\nPredictions Against madison", xlab="Class in madison bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=madison.pred, geom="violin", ylab="Model Score", xlab="Class in madison bootstrap", main="Combined Model Built Minus madison:\nPredictions Against madison")


bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSDNS 2011")
print(my.rf)
NSDNS.pred = predict(my.rf, NSDNS.2011.sparrow.samp, type="prob")
NSDNS.pred = data.frame(NSDNS.pred)
NSDNS.pred$Call_vs_Noise = NSDNS.2011.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSDNS.pred, main="Combined Model Built Minus NSDNS 2011:\nPredictions Against NSDNS 2011", xlab="Class in NSDNS 2011 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSDNS.pred, geom="violin", ylab="Model Score", xlab="Class in NSDNS 2011 bootstrap", main="Combined Model Built Minus NSDNS 2011:\nPredictions Against NSDNS 2011")

bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSDNS 2012")
print(my.rf)
NSDNS.pred = predict(my.rf, NSDNS.2012.sparrow.samp, type="prob")
NSDNS.pred = data.frame(NSDNS.pred)
NSDNS.pred$Call_vs_Noise = NSDNS.2012.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSDNS.pred, main="Combined Model Built Minus NSDNS 2012:\nPredictions Against NSDNS 2012", xlab="Class in NSDNS 2012 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSDNS.pred, geom="violin", ylab="Model Score", xlab="Class in NSDNS 2012 bootstrap", main="Combined Model Built Minus NSDNS 2012:\nPredictions Against NSDNS 2012")


bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSGDN 2011")
print(my.rf)
NSGDN.pred = predict(my.rf, NSGDN.2011.sparrow.samp, type="prob")
NSGDN.pred = data.frame(NSGDN.pred)
NSGDN.pred$Call_vs_Noise = NSGDN.2011.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSGDN.pred, main="Combined Model Built Minus NSGDN 2011:\nPredictions Against NSGDN 2011", xlab="Class in NSGDN 2011 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSGDN.pred, geom="violin", ylab="Model Score", xlab="Class in NSGDN 2011 bootstrap", main="Combined Model Built Minus NSGDN 2011:\nPredictions Against NSGDN 2011")


bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSGDN 2012")
print(my.rf)
NSGDN.pred = predict(my.rf, NSGDN.2012.sparrow.samp, type="prob")
NSGDN.pred = data.frame(NSGDN.pred)
NSGDN.pred$Call_vs_Noise = NSGDN.2012.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSGDN.pred, main="Combined Model Built Minus NSGDN 2012:\nPredictions Against NSGDN 2012", xlab="Class in NSGDN 2012 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSGDN.pred, geom="violin", ylab="Model Score", xlab="Class in NSGDN 2012 bootstrap", main="Combined Model Built Minus NSGDN 2012:\nPredictions Against NSGDN 2012")


bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSGDS 2011")
print(my.rf)
NSGDS.pred = predict(my.rf, NSGDS.2011.sparrow.samp, type="prob")
NSGDS.pred = data.frame(NSGDS.pred)
NSGDS.pred$Call_vs_Noise = NSGDS.2011.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSGDS.pred, main="Combined Model Built Minus NSGDS 2011:\nPredictions Against NSGDS 2011", xlab="Class in NSGDS 2011 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSGDS.pred, geom="violin", ylab="Model Score", xlab="Class in NSGDS 2011 bootstrap", main="Combined Model Built Minus NSGDS 2011:\nPredictions Against NSGDS 2011")

bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSGDS 2012")
print(my.rf)
NSGDS.pred = predict(my.rf, NSGDS.2012.sparrow.samp, type="prob")
NSGDS.pred = data.frame(NSGDS.pred)
NSGDS.pred$Call_vs_Noise = NSGDS.2012.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSGDS.pred, main="Combined Model Built Minus NSGDS 2012:\nPredictions Against NSGDS 2012", xlab="Class in NSGDS 2012 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSGDS.pred, geom="violin", ylab="Model Score", xlab="Class in NSGDS 2012 bootstrap", main="Combined Model Built Minus NSGDS 2012:\nPredictions Against NSGDS 2012")


bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSLVN 2012")
print(my.rf)
NSLVN.pred = predict(my.rf, NSLVN.2012.sparrow.samp, type="prob")
NSLVN.pred = data.frame(NSLVN.pred)
NSLVN.pred$Call_vs_Noise = NSLVN.2012.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSLVN.pred, main="Combined Model Built Minus NSLVN 2012:\nPredictions Against NSLVN 2012", xlab="Class in NSLVN 2012 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSLVN.pred, geom="violin", ylab="Model Score", xlab="Class in NSLVN 2012 bootstrap", main="Combined Model Built Minus NSLVN 2012:\nPredictions Against NSLVN 2012")

bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSNME 2011")
print(my.rf)
NSNME.pred = predict(my.rf, NSNME.2011.sparrow.samp, type="prob")
NSNME.pred = data.frame(NSNME.pred)
NSNME.pred$Call_vs_Noise = NSNME.2011.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSNME.pred, main="Combined Model Built Minus NSNME 2011:\nPredictions Against NSNME 2011", xlab="Class in NSNME 2011 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSNME.pred, geom="violin", ylab="Model Score", xlab="Class in NSNME 2011 bootstrap", main="Combined Model Built Minus NSNME 2011:\nPredictions Against NSNME 2011")

bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSNME 2012")
print(my.rf)
NSNME.pred = predict(my.rf, NSNME.2012.sparrow.samp, type="prob")
NSNME.pred = data.frame(NSNME.pred)
NSNME.pred$Call_vs_Noise = NSNME.2012.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSNME.pred, main="Combined Model Built Minus NSNME 2012:\nPredictions Against NSNME 2012", xlab="Class in NSNME 2012 bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSNME.pred, geom="violin", ylab="Model Score", xlab="Class in NSNME 2012 bootstrap", main="Combined Model Built Minus NSNME 2012:\nPredictions Against NSNME 2012")


bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, wyoming.sparrow.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out ontario")
print(my.rf)
ontario.pred = predict(my.rf, ontario.sparrow.samp, type="prob")
ontario.pred = data.frame(ontario.pred)
ontario.pred$Call_vs_Noise = ontario.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=ontario.pred, main="Combined Model Built Minus ontario:\nPredictions Against ontario", xlab="Class in ontario bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=ontario.pred, geom="violin", ylab="Model Score", xlab="Class in ontario bootstrap", main="Combined Model Built Minus ontario:\nPredictions Against ontario")

bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp)
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out wyoming")
print(my.rf)
wyoming.pred = predict(my.rf, wyoming.sparrow.samp, type="prob")
wyoming.pred = data.frame(wyoming.pred)
wyoming.pred$Call_vs_Noise = wyoming.sparrow.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=wyoming.pred, main="Combined Model Built Minus wyoming:\nPredictions Against wyoming", xlab="Class in wyoming bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=wyoming.pred, geom="violin", ylab="Model Score", xlab="Class in wyoming bootstrap", main="Combined Model Built Minus wyoming:\nPredictions Against wyoming")



bootstrap.all = rbind.fill(allegany.sparrow.samp, danby.sparrow.samp, lewis.sparrow.samp, madison.sparrow.samp, NSDNS.2011.sparrow.samp,  NSGDN.2011.sparrow.samp, NSGDS.2011.sparrow.samp, NSNME.2011.sparrow.samp, NSDNS.2012.sparrow.samp,  NSGDN.2012.sparrow.samp, NSGDS.2012.sparrow.samp, NSLVN.2012.sparrow.samp, NSNME.2012.sparrow.samp, ontario.sparrow.samp, wyoming.sparrow.samp )
combined.sparrow.model = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print(combined.sparrow.model)
