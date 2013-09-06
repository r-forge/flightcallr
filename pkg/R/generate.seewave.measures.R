generate.seewave.measures = function(
### Extract acoustic features from selections in a selection table. Requires
### that SoX (http://sox.sourceforge.net) be installed and callable as "soxi".
ubertable, 
### A selection table. In order to deal with selections which span sound files,
### generate.seewave.measures() can run soxi to tell the lengths of sound files.
### In this case, it is necessary that the "File Offset (s)" column be present,
### as well as either "Begin File" or "Begin Path".
sf_length, 
### As an alternative to running soxi, if all the sound files are of the same
### length, this can be specified (in seconds).
save_progress_file, 
### Name of file to save intermediate progress to. If absent, autosave is not
### done.
save_every=5000,
### How many rows to calculate before saving intermediate progress file.
sampling_rate=24000
### The sampling rate of the file(s) containing selections. Necessary in order to choose a suitable FFT window size.
) {
  require(tuneR)
  require(seewave)
  seewave.measures = c("Rugosity", "Crest_Factor", "Temporal_Entropy", "Shannon_Entropy", "Shannon_Entropy_Bandlimited", "Spectral_Flatness_Measure", 
                       "Spectral_Flatness_Measure_Bandlimited", "Spectrum_Roughness", "Spectrum_Roughness_Bandlimited", "Autocorrelation_Mean", 
                       "Autocorrelation_Median", "Autocorrelation_Standard_Error", "Dominant_Frequency_Mean", "Dominant_Frequency_Standard_Error", 
                       "Specprop_Mean", "Specprop_SD", "Specprop_SEM", "Specprop_Median", "Specprop_Mode", "Specprop_Q25", "Specprop_Q75", 
                       "Specprop_IQR", "Specprop_Cent", "Specprop_Skewness", "Specprop_Kurtosis", "Specprop_SFM", "Specprop_SH")
  std.error = function (x) sd(x, na.rm=T)/sqrt(sum(!is.na(x)))
  s.f = paste(ubertable$Sound_File_Path, ubertable$Sound_File_Name, sep="/")
  ubertable$s.f = s.f
  if (is.numeric(ubertable$Event_Offset)) {
    b.t = ubertable$Event_Offset
    e.t = ubertable$Event_Offset + ubertable$Event_Duration
  } else {
    b.t = ubertable$Begin.Time..s.
    e.t = ubertable$End.Time..s.
  }
  # setup - we need a list of lengths of the sound files, so we can skip selections which span files.
  files = unique(sort(s.f))
  lengths = numeric(length(files))
  names(lengths) = files
  if (missing(sf_length)) {
    for (i in 1:length(files)) {
      lengths[i] = as.numeric(system2("soxi", c("-D", shQuote(files[i])), stdout=T))
    }
  } else { 
    for (i in 1:length(files)) {
      lengths[i] = sf_length
    }
  }
  names(lengths) = files
  print(lengths)
  write.table(lengths, "lengths.csv")
  band = ubertable$Detector
  rugosity = crest.factor = temporal.entropy= shannon.entropy = shannon.entropy.bl = 
    spectral.flatness.measure = spectral.flatness.measure.bl = spectrum.roughness = 
    spectrum.roughness.bl = autoc.mean = autoc.median = autoc.se = dfreq.mean = dfreq.se = specprop.mean = specprop.sd = specprop.sem = 
    specprop.median = specprop.mode = specprop.Q25 = specprop.Q75 = specprop.IQR = specprop.cent = specprop.skewness = specprop.kurtosis =
    specprop.sfm = specprop.sh = numeric(nrow(ubertable))
  
  # should i use fftw in the below? no reason to bother so far, performance hit
  # is likely mainly file access
  for (i in 1:nrow(ubertable)) {   
    if (i %% 100 == 0) {
      cat(paste("i is ", i, "\n"))
      flush.console()
    }
    # save progress if so desired
    if (! missing(save_progress_file)) {
      if (i %% save_every == 0) {
        cat("Autosaving...\n")
        ubertable_so_far = ubertable
        ubertable.names = names(ubertable_so_far)
        ubertable_so_far = cbind(ubertable_so_far, rugosity, crest.factor, temporal.entropy, shannon.entropy, shannon.entropy.bl, spectral.flatness.measure, spectral.flatness.measure.bl, spectrum.roughness, spectrum.roughness.bl, autoc.mean, autoc.median, autoc.se, dfreq.mean, dfreq.se, specprop.mean, specprop.sd, specprop.sem, specprop.median, specprop.mode, specprop.Q25, specprop.Q75, specprop.IQR, specprop.cent, specprop.skewness, specprop.kurtosis, specprop.sfm, specprop.sh)
        names(ubertable_so_far) = c(ubertable.names, seewave.measures)
        save(ubertable_so_far, file=save_progress_file)
      }
    }
    
    # drop any events less than two DFT windows long. N.B. this is specific to 
    # 24 kHz sampling rate; should be replaced with code which does calculation
    # based on actual sampling rate of files!`
    
    # I am also NAing any events which span file boundaries in the whole-deployment sound stream. This could probably be replaced with
    # code to do the actual spanning without a huge deal of trouble, and I'll have to do that once we're analyzing shorter files. But for
    # whole-night NFC files, there won't be any events spanning file boundaries.
    if (ubertable[i, "Event_Duration"] <= (1 / sampling_rate) * 128 * 2 || 
          ubertable[i, "Event_Offset"] + ubertable[i, "Event_Duration"] >  lengths[ubertable[i, "s.f"]]) {
      cat(paste("i is ", i, "\n"))
      flush.console()
      if (ubertable[i, "Event_Duration"] <= (1 / sampling_rate) * 128 * 2) {
        cat(paste("Event is too short:", ubertable[i, "Event_Duration"], "seconds.", "\n"))
        flush.console()
      } else {
        cat(paste("Event overlaps sound files.", "\n"))
        flush.console()
      }
      rugosity[i] = crest.factor[i] = temporal.entropy[i] = shannon.entropy[i] = shannon.entropy.bl[i] = spectral.flatness.measure[i] =
        spectral.flatness.measure.bl[i] = spectrum.roughness[i] = spectrum.roughness.bl[i] = autoc.mean[i] = autoc.median[i] =
        autoc.se[i] = dfreq.mean[i] = dfreq.se[i] = specprop.mean[i] = specprop.sd[i] = specprop.sem[i] = specprop.median[i] =
        specprop.mode[i] = specprop.Q25[i] = specprop.Q75[i] = specprop.cent[i] = specprop.skewness[i] = specprop.kurtosis[i] =
        specprop.sfm[i] = specprop.sh[i] = NA
    } else {
      foo = readWave(s.f[i], from=b.t[i], to=e.t[i], units="seconds")
      # trying PSD - it seems to give clearer plots
      #foo.spec = spec(foo, plot=F, wl=128)
      foo.spec = spec(foo, plot=F, wl=sampling_rate * (128/24000), PSD=T)
      # don't need to bandlimit myself, specprop has flim - probably remove this and
      # associated measures
      foo.spec.bl = bandlimit.spectrum(foo.spec, band[i])
      if (band[i] == "Thrush") {
        upper = 3.75
        lower = 2.25
      } else if (band[i] == "Sparrow") {
        upper = 11
        lower = 6
      } else if (band[i] == "gunshot") {
        upper = 2
        lower = 0
      } else {
        print(paste("hmm,", band, "is not a recognized band"))
      }
      foo.specprop = specprop(foo.spec, flim=c(lower, upper))
      # some other library is stinking up env(), call it by namespace explicitly
      foo.env = seewave:::env(foo, plot=F) 
      foo.meanspec = meanspec(foo, plot=FALSE, wl=sampling_rate * (128/24000), ovlp=90)
      foo.meanspec.bl = bandlimit.spectrum(foo.meanspec, band[i])
      foo.autoc = autoc(foo, wl=sampling_rate * (128/24000), plot=F)
      foo.dfreq = dfreq(foo, wl=sampling_rate * (128/24000), plot=F, ovlp=90)
      rugosity[i] = rugo(foo@left / max(foo@left))
      crest.factor[i] = crest(foo)$C
      temporal.entropy[i] = th(foo.env)
      shannon.entropy[i] = sh(foo.spec)
      shannon.entropy.bl[i] = sh(foo.spec.bl)
      spectral.flatness.measure[i] = sfm(foo.spec)
      spectral.flatness.measure.bl[i] = sfm(foo.spec.bl)
      spectrum.roughness[i] = roughness(foo.meanspec[,2])
      spectrum.roughness.bl[i] = roughness(foo.meanspec.bl[,2])
      autoc.mean[i] = mean(foo.autoc[,2], na.rm=T)
      autoc.median[i] = median(foo.autoc[,2], na.rm=T)
      autoc.se[i] = std.error(foo.autoc[,2], na.rm=T)
      dfreq.mean[i] = mean(foo.dfreq[,2], na.rm=T)
      dfreq.se[i] = std.error(foo.dfreq[,2], na.rm=T)
      specprop.mean[i] = foo.specprop$mean
      specprop.sd[i] = foo.specprop$sd
      specprop.sem[i] = foo.specprop$sem
      specprop.median[i] = foo.specprop$median
      specprop.mode[i] = foo.specprop$mode
      specprop.Q25[i] = foo.specprop$Q25
      specprop.Q75[i] = foo.specprop$Q75
      specprop.IQR[i] = foo.specprop$IQR
      specprop.cent[i] = foo.specprop$cent
      specprop.skewness[i] = foo.specprop$skewness
      specprop.kurtosis[i] = foo.specprop$kurtosis
      specprop.sfm[i] = foo.specprop$sfm
      specprop.sh[i] = foo.specprop$sh
    }
  }
  ubertable.names = names(ubertable)
  ubertable = cbind(ubertable, rugosity, crest.factor, temporal.entropy, shannon.entropy, shannon.entropy.bl, spectral.flatness.measure, spectral.flatness.measure.bl, spectrum.roughness, spectrum.roughness.bl, autoc.mean, autoc.median, autoc.se, dfreq.mean, dfreq.se, specprop.mean, specprop.sd, specprop.sem, specprop.median, specprop.mode, specprop.Q25, specprop.Q75, specprop.IQR, specprop.cent, specprop.skewness, specprop.kurtosis, specprop.sfm, specprop.sh)
  names(ubertable) = c(ubertable.names, seewave.measures)
  print("still alive 4")
  ubertable$Detector_numeric = as.numeric(ubertable$Detector)
  return(ubertable)
### The original selection table, with columns added for generated features.
}

