require(plyr)
require(flightcallr)
require(randomForest)

read.selection.tables = function() {
  allegany.thrush.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ALLEGANY_20120914_185000.THRUSH_GET_CTHDATA_93517selections.1-5_DAC_aek.txt", Sound_File_Path=".")
  allegany.thrush.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ALLEGANY_20120914_185000.THRUSH_GET_CTHDATA_93517selections.6-10_DAC_aek.txt", Sound_File_Path = ".")
  allegany.thrush.11to15 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/ALLEGANY_20120914_185000.THRUSH_GET_CTHDATA_93517selections.11-15_DAC_aek.txt", Sound_File_Path = ".")
  allegany.thrush.bootstrap = rbind.fill(danby.thrush.1to5, danby.thrush.6to10, danby.thrush.11to15)
  load("random_forest/nyserda/allegany.thrush.rda")
  allegany.thrush = allegany.thrush[allegany.thrush$Random.Percent <= 15, ]
  allegany.thrush = allegany.thrush[order(allegany.thrush$Selection), ]
  allegany.thrush.bootstrap = allegany.thrush.bootstrap[order(allegany.thrush.bootstrap$Selection), ]
  allegany.thrush$Call_vs_Noise = allegany.thrush.bootstrap$Call_vs_Noise
  allegany.thrush.bootstrap = allegany.thrush

  danby.thrush.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/DANBY-_20120914_184500.THRUSH_GET_CTHDATA_109751selections.1-5_DAC_aek.txt", Sound_File_Path = ".")
  danby.thrush.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/DANBY-_20120914_184500.THRUSH_GET_CTHDATA_109751selections.6-10_DAC_aek.txt", Sound_File_Path = ".")
  danby.thrush.11to15 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/DANBY-_20120914_184500.THRUSH_GET_CTHDATA_109751selections.11-15_DAC_aek.txt", Sound_File_Path = ".")
  danby.thrush.bootstrap = rbind.fill(danby.thrush.1to5, danby.thrush.6to10, danby.thrush.11to15)
  load("random_forest/nyserda/danby.thrush.rda")
  danby.thrush = danby.thrush[danby.thrush$Random.Percent <= 15, ]
  danby.thrush = danby.thrush[order(danby.thrush$Selection), ]
  danby.thrush.bootstrap = danby.thrush.bootstrap[order(danby.thrush.bootstrap$Selection), ]
  danby.thrush$Call_vs_Noise = danby.thrush.bootstrap$Call_vs_Noise
  danby.thrush.bootstrap = danby.thrush


  madison.thrush.1to5 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/MADISON_20120914_184200.THRUSH_GET_CTHDATA_437994selections.1-5_DAC_aek.txt", Sound_File_Path= ".")
  madison.thrush.6to10 = read.selection.table("p:/2012_NYSERDA/040_Bootstrap/030_Vetted/MADISON_20120914_184200.THRUSH_GET_CTHDATA_437994selections.6-10_DAC_aek.txt", Sound_File_Path = ".")
  madison.thrush.bootstrap = rbind.fill(madison.thrush.1to5, madison.thrush.6to10)
  load("random_forest/nyserda/madison.thrush.rda")
  madison.thrush = madison.thrush[madison.thrush$Random.Percent <= 10, ]
  madison.thrush = madison.thrush[order(madison.thrush$Selection), ]
  madison.thrush.bootstrap = madison.thrush.bootstrap[order(madison.thrush.bootstrap$Selection), ]
  madison.thrush$Call_vs_Noise = madison.thrush.bootstrap$Call_vs_Noise
  madison.thrush.bootstrap = madison.thrush
}

#
#allegany.thrush.call = allegany.thrush.bootstrap[allegany.thrush.bootstrap$Call_vs_Noise == "Call", ]
#allegany.thrush.noise = allegany.thrush.bootstrap[allegany.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(allegany.thrush.call)
#allegany.thrush.noise = allegany.thrush.noise[sample(nrow(allegany.thrush.noise), 10 * num.calls), ]
#allegany.thrush.samp = rbind(allegany.thrush.call, allegany.thrush.noise)
#
#danby.thrush.call = danby.thrush.bootstrap[danby.thrush.bootstrap$Call_vs_Noise == "Call", ]
#danby.thrush.noise = danby.thrush.bootstrap[danby.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(danby.thrush.call)
#danby.thrush.noise = danby.thrush.noise[sample(nrow(danby.thrush.noise), 10 * num.calls), ]
#danby.thrush.samp = rbind(danby.thrush.call, danby.thrush.noise)
#
#lewis.thrush.call = lewis.thrush.bootstrap[lewis.thrush.bootstrap$Call_vs_Noise == "Call", ]
#lewis.thrush.noise = lewis.thrush.bootstrap[lewis.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(lewis.thrush.call)
#lewis.thrush.noise = lewis.thrush.noise[sample(nrow(lewis.thrush.noise), 10 * num.calls), ]
#lewis.thrush.samp = rbind(lewis.thrush.call, lewis.thrush.noise)
#
#madison.thrush.call = madison.thrush.bootstrap[madison.thrush.bootstrap$Call_vs_Noise == "Call", ]
#madison.thrush.noise = madison.thrush.bootstrap[madison.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(madison.thrush.call)
#madison.thrush.noise = madison.thrush.noise[sample(nrow(madison.thrush.noise), 10 * num.calls), ]
#madison.thrush.samp = rbind(madison.thrush.call, madison.thrush.noise)
#
#NSDNS.thrush.call = NSDNS.thrush.bootstrap[NSDNS.thrush.bootstrap$Call_vs_Noise == "Call", ]
#NSDNS.thrush.noise = NSDNS.thrush.bootstrap[NSDNS.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(NSDNS.thrush.call)
#NSDNS.thrush.noise = NSDNS.thrush.noise[sample(nrow(NSDNS.thrush.noise), 10 * num.calls), ]
#NSDNS.thrush.samp = rbind(NSDNS.thrush.call, NSDNS.thrush.noise)
#
#NSGDN.thrush.call = NSGDN.thrush.bootstrap[NSGDN.thrush.bootstrap$Call_vs_Noise == "Call", ]
#NSGDN.thrush.noise = NSGDN.thrush.bootstrap[NSGDN.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(NSGDN.thrush.call)
#NSGDN.thrush.noise = NSGDN.thrush.noise[sample(nrow(NSGDN.thrush.noise), 10 * num.calls), ]
#NSGDN.thrush.samp = rbind(NSGDN.thrush.call, NSGDN.thrush.noise)
#
#NSGDS.thrush.call = NSGDS.thrush.bootstrap[NSGDS.thrush.bootstrap$Call_vs_Noise == "Call", ]
#NSGDS.thrush.noise = NSGDS.thrush.bootstrap[NSGDS.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(NSGDS.thrush.call)
#NSGDS.thrush.noise = NSGDS.thrush.noise[sample(nrow(NSGDS.thrush.noise), 10 * num.calls), ]
#NSGDS.thrush.samp = rbind(NSGDS.thrush.call, NSGDS.thrush.noise)
#
#NSNME.thrush.call = NSNME.thrush.bootstrap[NSNME.thrush.bootstrap$Call_vs_Noise == "Call", ]
#NSNME.thrush.noise = NSNME.thrush.bootstrap[NSNME.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(NSNME.thrush.call)
#NSNME.thrush.noise = NSNME.thrush.noise[sample(nrow(NSNME.thrush.noise), 10 * num.calls), ]
#NSNME.thrush.samp = rbind(NSNME.thrush.call, NSNME.thrush.noise)
#
#ontario.thrush.call = ontario.thrush.bootstrap[ontario.thrush.bootstrap$Call_vs_Noise == "Call", ]
#ontario.thrush.noise = ontario.thrush.bootstrap[ontario.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(ontario.thrush.call)
#ontario.thrush.noise = ontario.thrush.noise[sample(nrow(ontario.thrush.noise), 10 * num.calls), ]
#ontario.thrush.samp = rbind(ontario.thrush.call, ontario.thrush.noise)
#
#wyoming.thrush.call = wyoming.thrush.bootstrap[wyoming.thrush.bootstrap$Call_vs_Noise == "Call", ]
#wyoming.thrush.noise = wyoming.thrush.bootstrap[wyoming.thrush.bootstrap$Call_vs_Noise == "Noise", ]
#num.calls = nrow(wyoming.thrush.call)
#wyoming.thrush.noise = wyoming.thrush.noise[sample(nrow(wyoming.thrush.noise), 10 * num.calls), ]
#wyoming.thrush.samp = rbind(wyoming.thrush.call, wyoming.thrush.noise)
#
#rm(list = ls(pattern="(call|noise)$"))




bootstrap.all = rbind.fill(danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out allegany")
print(my.rf)
allegany.pred = predict(my.rf, allegany.thrush.samp, type="prob")
allegany.pred = data.frame(allegany.pred)
allegany.pred$Call_vs_Noise = allegany.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=allegany.pred, main="Combined Model Built Minus allegany:\nPredictions Against allegany", xlab="Class in allegany bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=allegany.pred, geom="violin", ylab="Model Score", xlab="Class in allegany bootstrap", main="Combined Model Built Minus allegany:\nPredictions Against allegany")

bootstrap.all = rbind.fill( allegany.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out danby")
print(my.rf)
danby.pred = predict(my.rf, danby.thrush.samp, type="prob")
danby.pred = data.frame(danby.pred)
danby.pred$Call_vs_Noise = danby.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=danby.pred, main="Combined Model Built Minus danby:\nPredictions Against danby", xlab="Class in danby bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=danby.pred, geom="violin", ylab="Model Score", xlab="Class in danby bootstrap", main="Combined Model Built Minus danby:\nPredictions Against danby")

bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out lewis")
print(my.rf)
lewis.pred = predict(my.rf, lewis.thrush.samp, type="prob")
lewis.pred = data.frame(lewis.pred)
lewis.pred$Call_vs_Noise = lewis.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=lewis.pred, main="Combined Model Built Minus lewis:\nPredictions Against lewis", xlab="Class in lewis bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=lewis.pred, geom="violin", ylab="Model Score", xlab="Class in lewis bootstrap", main="Combined Model Built Minus lewis:\nPredictions Against lewis")

bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out madison")
print(my.rf)
madison.pred = predict(my.rf, madison.thrush.samp, type="prob")
madison.pred = data.frame(madison.pred)
madison.pred$Call_vs_Noise = madison.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=madison.pred, main="Combined Model Built Minus madison:\nPredictions Against madison", xlab="Class in madison bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=madison.pred, geom="violin", ylab="Model Score", xlab="Class in madison bootstrap", main="Combined Model Built Minus madison:\nPredictions Against madison")


bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSDNS")
print(my.rf)
NSDNS.pred = predict(my.rf, NSDNS.thrush.samp, type="prob")
NSDNS.pred = data.frame(NSDNS.pred)
NSDNS.pred$Call_vs_Noise = NSDNS.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSDNS.pred, main="Combined Model Built Minus NSDNS:\nPredictions Against NSDNS", xlab="Class in NSDNS bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSDNS.pred, geom="violin", ylab="Model Score", xlab="Class in NSDNS bootstrap", main="Combined Model Built Minus NSDNS:\nPredictions Against NSDNS")

bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSGDN")
print(my.rf)
NSGDN.pred = predict(my.rf, NSGDN.thrush.samp, type="prob")
NSGDN.pred = data.frame(NSGDN.pred)
NSGDN.pred$Call_vs_Noise = NSGDN.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSGDN.pred, main="Combined Model Built Minus NSGDN:\nPredictions Against NSGDN", xlab="Class in NSGDN bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSGDN.pred, geom="violin", ylab="Model Score", xlab="Class in NSGDN bootstrap", main="Combined Model Built Minus NSGDN:\nPredictions Against NSGDN")

bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSGDS")
print(my.rf)
NSGDS.pred = predict(my.rf, NSGDS.thrush.samp, type="prob")
NSGDS.pred = data.frame(NSGDS.pred)
NSGDS.pred$Call_vs_Noise = NSGDS.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSGDS.pred, main="Combined Model Built Minus NSGDS:\nPredictions Against NSGDS", xlab="Class in NSGDS bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSGDS.pred, geom="violin", ylab="Model Score", xlab="Class in NSGDS bootstrap", main="Combined Model Built Minus NSGDS:\nPredictions Against NSGDS")

bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, ontario.thrush.samp, wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out NSNME")
print(my.rf)
NSNME.pred = predict(my.rf, NSNME.thrush.samp, type="prob")
NSNME.pred = data.frame(NSNME.pred)
NSNME.pred$Call_vs_Noise = NSNME.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=NSNME.pred, main="Combined Model Built Minus NSNME:\nPredictions Against NSNME", xlab="Class in NSNME bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=NSNME.pred, geom="violin", ylab="Model Score", xlab="Class in NSNME bootstrap", main="Combined Model Built Minus NSNME:\nPredictions Against NSNME")

bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  wyoming.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out ontario")
print(my.rf)
ontario.pred = predict(my.rf, ontario.thrush.samp, type="prob")
ontario.pred = data.frame(ontario.pred)
ontario.pred$Call_vs_Noise = ontario.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=ontario.pred, main="Combined Model Built Minus ontario:\nPredictions Against ontario", xlab="Class in ontario bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=ontario.pred, geom="violin", ylab="Model Score", xlab="Class in ontario bootstrap", main="Combined Model Built Minus ontario:\nPredictions Against ontario")

bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp )
my.rf = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print("leaving out wyoming")
print(my.rf)
wyoming.pred = predict(my.rf, wyoming.thrush.samp, type="prob")
wyoming.pred = data.frame(wyoming.pred)
wyoming.pred$Call_vs_Noise = wyoming.thrush.samp$Call_vs_Noise
windows()
boxplot(Noise ~ Call_vs_Noise, data=wyoming.pred, main="Combined Model Built Minus wyoming:\nPredictions Against wyoming", xlab="Class in wyoming bootstrap", ylab="Model Score")
windows(); qplot(Call_vs_Noise, Noise, data=wyoming.pred, geom="violin", ylab="Model Score", xlab="Class in wyoming bootstrap", main="Combined Model Built Minus wyoming:\nPredictions Against wyoming")



bootstrap.all = rbind.fill( allegany.thrush.samp, danby.thrush.samp, lewis.thrush.samp, madison.thrush.samp, NSDNS.thrush.samp,  NSGDN.thrush.samp, NSGDS.thrush.samp, NSNME.thrush.samp,  ontario.thrush.samp, wyoming.thrush.samp )
combined.thrush.model = randomForest(Call_vs_Noise ~ ., data=bootstrap.all[, c(seewave.measures, "Call_vs_Noise", "Event_Duration")], na.action=na.omit)
print(combined.thrush.model)
