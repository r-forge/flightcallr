save.selection.table = function(
### Save a selection table to a file.
### Multi-line documentation!
selection.table = "", 
### The selection table data frame.
filename = "", 
### The desired filename
path = "", 
### The desired path
include.measures = F, 
### Include generated features in the table?
include.call.vs.noise = F,
### Include the Call_vs_Noise column?
include.predictions = F
### Include prediction columns RF_Score and RF_Prediction?
) {
  # save a selection table!
  seewave.measures = c("Rugosity", "Crest_Factor", "Temporal_Entropy", "Shannon_Entropy", "Shannon_Entropy_Bandlimited", "Spectral_Flatness_Measure", 
                       "Spectral_Flatness_Measure_Bandlimited", "Spectrum_Roughness", "Spectrum_Roughness_Bandlimited", "Autocorrelation_Mean", 
                       "Autocorrelation_Median", "Autocorrelation_Standard_Error", "Dominant_Frequency_Mean", "Dominant_Frequency_Standard_Error", 
                       "Specprop_Mean", "Specprop_SD", "Specprop_SEM", "Specprop_Median", "Specprop_Mode", "Specprop_Q25", "Specprop_Q75", 
                       "Specprop_IQR", "Specprop_Cent", "Specprop_Skewness", "Specprop_Kurtosis", "Specprop_SFM", "Specprop_SH")
  if (! include.measures) {
    selection.table = selection.table[ , ! names(selection.table) %in% seewave.measures]
  } 
  if (! include.call.vs.noise) {
    selection.table = selection.table[ , ! names(selection.table) %in% c("Call_vs_Noise")]
  }
  if (! include.predictions) {
    selection.table = selection.table[ , ! names(selection.table) %in% c("RF_Score", "RF_Prediction")]
  }
  selection.table = selection.table[ , ! names(selection.table) %in% c("Detector_numeric")]
  my.path = paste(path, filename, sep="/")
  options(digits = 12)
  names(selection.table) = gsub("\\.s\\.", "(s)", names(selection.table))
  names(selection.table) = gsub("\\.Hz\\.", "(s)", names(selection.table))
  names(selection.table) = gsub("\\.", " ", names(selection.table))
  write.table(selection.table, file=my.path, row.names=F, sep="\t", quote=F)
### The return value is the same as that of write.table()
}
