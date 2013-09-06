read.selection.table = function (
### Read a Raven selection table into a data frame.
filename, 
### The selection table to read.
Sound_File_Path, 
### The path of the sound file(s) referred to in the selection table. If the
### Begin Path measurement is present in the selection table, it is not 
### necessary to include this argument. If sound files are at multiple paths,
### it is necessary to include the Begin Path measurement instead of using this
### argument.
detector
# Detector can be specified manually if it's not present in the selection table
) {
  ubertable = read.csv(filename, header=T, sep="\t")
  if (missing(Sound_File_Path)) {
    if ("Begin.Path" %in% names(ubertable)) {
      ubertable$Sound_File_Path = sub("(.*)\\\\.*", "\\1", ubertable$Begin.Path)
    } else {
      stop("can't figure out how to calculate the sound file paths")
    }
  } else {
    ubertable$Sound_File_Path = Sound_File_Path
  }
  ubertable$Sound_File_Path = gsub("\\\\", "/", ubertable$Sound_File_Path)
  ubertable$Sound_File_Path = sub("/$", "", ubertable$Sound_File_Path)
  ubertable$Sound_File_Name = ubertable$Begin.File
  ubertable$Event_Duration = ubertable$End.Time..s. - ubertable$Begin.Time..s.
  ubertable$Event_Offset = ubertable$File.Offset..s.
  if (missing(detector)) {
    thrush = grepl("^(GETCTH|THRUSH_GET|Thrush)", ubertable$Detector)
    sparrow = grepl("^(Sp100|Sparrow)", ubertable$Detector)
    gunshot = grepl("^(gunshot)", ubertable$Detector)
    NARW = grepl("^(NARW)", ubertable$Detector)
    detector.new = rep("", length(ubertable$Detector))
    detector.new[sparrow] = "Sparrow"
    detector.new[thrush] = "Thrush"
    detector.new[gunshot] = "gunshot"
    detector.new[NARW] = "NARW"
    detector.new = factor(detector.new)
    ubertable$Detector = detector.new
    keep = sparrow | thrush | gunshot | NARW
    ubertable.new = ubertable[keep,]
    ubertable.new$Detector = factor(ubertable.new$Detector)
  } else {
    ubertable.new = ubertable
    ubertable.new$Detector = detector
  }
  if ("Calls" %in% names(ubertable.new)) { # downcase the column header
    if ("calls" %in% names(ubertable.new)) {
      stop("My mind is blown because of 'calls' and 'Calls' columns in same table.")
    }
    ubertable.new$calls = ubertable.new$Calls
    ubertable.new = ubertable.new[, ! names(ubertable.new) %in% c("Calls")]
  }
  if ( ! "calls" %in% names(ubertable.new)) {  # there will be a "calls" column if Anne has done bootstrap browsing
    print("option 1")
    ubertable.new$Call_vs_Noise = rep("", nrow(ubertable.new))
  } else{ # caution - this could fill the Call_vs_Noise column with spurious "Noise" entries - shouldn't be a problem working w/ non-bootstrapped files
    print("option 2")
    ubertable.new$Call_vs_Noise = ifelse(ubertable.new$calls == "NFC", "Call", "Noise")
    ubertable.new$Call_vs_Noise = factor(ubertable.new$Call_vs_Noise)
  }
  return(ubertable.new)
### The data frame representation of the selection table.
}
