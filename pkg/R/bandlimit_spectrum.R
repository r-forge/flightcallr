bandlimit.spectrum = function(
### This function takes a spectrum matrix and chops off the rows which are not within the specified band.
### It's not clear that it's useful overall, since seewave's specprop() has the ability to do band-
### limiting. Nevertheless we're holding on to it until we can verify that it doesn't provide anything 
### useful
spectrum, 
### The spectrum to be band-limited.
band
### The name of the band, either "Sparrow" or "Thrush".
) {
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
### The band-limited spectrum.
}

