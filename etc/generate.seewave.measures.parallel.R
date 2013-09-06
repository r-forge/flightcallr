generate.seewave.measures.parallel = function(ubertable = sample.ubertable) {
  
  # This is a wrapper to allow me to use sfLapply - we want to accept an index and perform the computations for 
  # the event in that row of the data frame.
  # We return a single row including the new columns.
  # We assume the existence of global variables ubertable, s.f, b.t., and e.t.
  
  
  # This function takes a spectrum matrix and chops off the rows which are not within the specified band.
  # It's not clear that it's useful overall, since seewave's specprop() has the ability to do band-
  # limiting. Nevertheless we're holding on to it until we can verify that it doesn't provide anything 
  # useful
  bandlimit.spectrum = function(spectrum, band) {
    if (band == "Thrush") {
      upper = 3.75
      lower = 2.25
    } else if (band == "Sparrow") {
      upper = 11
      lower = 6
    } else {
      print(paste("hmm,", band, "is not a recognized band"))
    }
    spectrum.lowest = spectrum[1,1]
    spectrum.bl = spectrum[(spectrum[,1] < upper & spectrum[,1] > lower),]
    spectrum.bl[,1] = spectrum.bl[,1] - min(spectrum.bl[,1])
    spectrum.bl[,1] = spectrum.bl[,1] + spectrum.lowest
    return(spectrum.bl)
  }
  
  generate.single.measurement.set = function (i) {
    # drop any events less than two DFT windows long. N.B. this is specific to 
    # 24 kHz sampling rate; should be replaced with code which does calculation
    # based on actual sampling rate of files!`
    
    # I am also NAing any events which span file boundaries in the whole-deployment sound stream. This could probably be replaced with
    # code to do the actual spanning without a huge deal of trouble, and I'll have to do that once we're analyzing shorter files. But for
    # whole-night NFC files, there won't be any events spanning file boundaries.
    my.row = ubertable[i, ]
    old.names = names(my.row)
    if (my.row$Event_Duration <= 0.01067 || my.row$Event_Offset + my.row$Event_Duration > lengths[my.row$Sound_File_Name]) {
      cat(paste("i is ", i, "\n"))
      flush.console()
      if (my.row$Event_Duration <= 0.01067) {
        cat(paste("Event is too short:", my.row$Event_Duration, "seconds.", "\n"))
        flush.console()
      } else {
        cat(paste("Event overlaps sound files.", "\n"))
        flush.console()
      }
      my.row = c(my.row, rep(NA, length(seewave.measures)))   # NAs for everything; can't compute this row
    } else {
      foo = readWave(s.f[i], from=b.t[i], to=e.t[i], units="seconds")
      foo.specdB = spec(foo, dB="max0", plot=F, wl=128)
      foo.specdB.bl = bandlimit.spectrum(foo.specdB, band[i])
      # trying PSD - it seems to give clearer plots
      #foo.spec = spec(foo, plot=F, wl=128)
      foo.spec = spec(foo, plot=F, wl=128, PSD=T)
      # don't need to bandlimit myself, specprop has flim - probably remove this and
      # associated measures
      foo.spec.bl = bandlimit.spectrum(foo.spec, band[i])
      if (band[i] == "Thrush") {
        upper = 3.75
        lower = 2.25
      } else if (band[i] == "Sparrow") {
        upper = 11
        lower = 6
      } else {
        print(paste("hmm,", band, "is not a recognized band"))
      }
      foo.specprop = specprop(foo.spec, flim=c(lower, upper))
      # some other library is stinking up env(), call it by namespace explicitly
      foo.env = seewave:::env(foo, plot=F) 
      foo.meanspec = meanspec(foo, plot=FALSE, wl=128, ovlp=90)
      foo.meanspec.bl = bandlimit.spectrum(foo.meanspec, band[i])
      foo.autoc = autoc(foo, wl=128, plot=F)
      foo.dfreq = dfreq(foo, wl=128, plot=F, ovlp=90)
      my.row = c(my.row, rugo(foo@left / max(foo@left)), crest(foo)$C, th(foo.env), sh(foo.spec), sh(foo.spec.bl), sfm(foo.spec), sfm(foo.spec.bl),
                 roughness(foo.meanspec[,2]), roughness(foo.meanspec.bl[,2]), mean(foo.autoc[,2], na.rm=T), median(foo.autoc[,2], na.rm=T),
                 std.error(foo.autoc[,2], na.rm=T), mean(foo.dfreq[,2], na.rm=T), std.error(foo.dfreq[,2], na.rm=T), foo.specprop$mean,
                 foo.specprop$sd, foo.specprop$sem, foo.specprop$median, foo.specprop$mode, foo.specprop$Q25, foo.specprop$Q75, foo.specprop$IQR,
                 foo.specprop$cent, foo.specprop$skewness, foo.specprop$kurtosis, foo.specprop$sfm, foo.specprop$sh)
    }
    names(my.row) = c(old.names, seewave.measures)
    my.row
  }
  require(tuneR)
  require(seewave)
  require(plotrix)
  require(snowfall)
  require(plyr)
  seewave.measures = c("Rugosity", "Crest_Factor", "Temporal_Entropy", "Shannon_Entropy", "Shannon_Entropy_Bandlimited", "Spectral_Flatness_Measure", 
                       "Spectral_Flatness_Measure_Bandlimited", "Spectrum_Roughness", "Spectrum_Roughness_Bandlimited", "Autocorrelation_Mean", 
                       "Autocorrelation_Median", "Autocorrelation_Standard_Error", "Dominant_Frequency_Mean", "Dominant_Frequency_Standard_Error", 
                       "Specprop_Mean", "Specprop_SD", "Specprop_SEM", "Specprop_Median", "Specprop_Mode", "Specprop_Q25", "Specprop_Q75", 
                       "Specprop_IQR", "Specprop_Cent", "Specprop_Skewness", "Specprop_Kurtosis", "Specprop_SFM", "Specprop_SH")
  # when I have time, it would be nice to remove the plotrix dependency, which is only for the silly std.error() function.
  # However, the function I've written below deals with NAs in an unclear way, and I don't want to introduce any functional
  # changes. jcr 2012-10-01
  #std.error = function (x) sqrt(var(x)/length(x))
  s.f = paste(ubertable$Sound_File_Path, ubertable$Sound_File_Name, sep="/")
  if (is.numeric(ubertable$Event_Offset)) {
    b.t = ubertable$Event_Offset
    e.t = ubertable$Event_Offset + ubertable$Event_Duration
  } else {
    b.t = ubertable$Begin.Time..s.
    e.t = ubertable$End.Time..s.
  }
  # setup - we need a list of lengths of the sound files, so we can skip selections which span files.
  # N.B. the line below will only work if everything is in a single sound file directory!
  files = paste(names(table(ubertable$Sound_File_Path)), names(table(ubertable$Sound_File_Name)), sep="/")
  lengths = rep(NA, length(files))
  names(lengths) = files
  for (i in 1:length(files)) {
    lengths[i] = as.numeric(system2("soxi", c("-D", shQuote(files[i])), stdout=T))
  }
  names(lengths) = files
  print(lengths)
  band = ubertable$Detector
    
  
  sfInit(parallel=T, cpus=4, type="SOCK", slaveOutfile="c:/parallel.out.txt")
  
  sfExport("lengths", "e.t", "b.t", "s.f", "seewave.measures", "ubertable")
  sfLibrary(tuneR)
  sfLibrary(seewave)
  sfLibrary(plotrix)
  foo = sfLapply(seq_len(nrow(ubertable)), generate.single.measurement.set)
  ubertable = ldply(foo, data.frame)
  ubertable$Detector_numeric = as.numeric(ubertable$Detector)
  sfStop()
  return(ubertable)
  
  

}