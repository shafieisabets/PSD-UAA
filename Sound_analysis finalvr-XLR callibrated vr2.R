#install.packages('devtools')
#require(devtools)
#install_git("https://gitlab.com/RTbecard/ezPSD.git", build_vignettes = T, force = T)
require(ezPSD)
#also the source code for the ezPSD package needs to be downloaded...

#install.packages('ggplot2')
require(ggplot2)

# Load recording
setwd('J:/MUSIC')
signal.raw <- read.wav('230508_1776.wav')

# Enter calibration constants
calib.hydrophone <- -158 # AS-1: -208; AS-1 + PA4: -158
calib.recorder <- -5.67 # lvl 1: 25.36; lvl 4: 15.28; lvl 10: -5.67
calib.correction <- 16.5 # for using the XLR port rather than the audio jack

# Sample rate
fs <- 44100

## Calibrate signal
signal.calibrated <- calibrateSignal.Time(data = signal.raw[[1]],fs = fs,
  calib = (calib.hydrophone - calib.recorder - calib.correction))

# Duration of the recording
Duration <- length(signal.calibrated)/fs # seconds

# Waveform
WF <- data.frame('sample' = 1:length(signal.calibrated), 'power' = signal.calibrated)
ggplot(data = WF, aes(x = sample/fs, y = 20*log10(power))) +
  geom_line(size = 2) +
  ylab(expression(paste("SPL (dB re 1 ",mu,"Pa"^"2","/Hz)")))+
  xlab("Time (s)")+
  theme_bw()

# Make PSD
welch <- ezWelch(signal.calibrated, wl = (1024*6), olap = 0.5, fs = fs, windowType = 'Hann')
message('# of overlapping Window Segments: ',nrow(welch$WindowSegments))

welch.df <- data.frame('Frequency' = welch$Frequency, 'PSD' = welch$PSD)
ggplot(data = welch.df, aes(x = welch.df$Frequency, y = 10*log10(welch.df$PSD))) +
  geom_line() +
  xlim(0,3000) +
  ylab(expression(paste("SPL (dB re 1 ",mu,"Pa"^"2","/Hz)")))+
  xlab("Frequency (Hz)")+
  theme_bw()
  
# Apply bandpass filter
bandpass.range <- c(100,1000)
signal.bandpassed <- ezButter(signal.calibrated, order = 5, freq = bandpass.range)

## Calculate SPL 

# From the bandpassed waveform
ref <- 1 # reference value of 1uPa for underwater measurements
20*log10(ezPSD::rms(signal.bandpassed)/ref)

# From the non-bandpassed waveform
20*log10(ezPSD::rms(signal.calibrated)/ref)

## Calculate SPL from the PSD, also not bandpassed!
10*log10(sum(welch$Power)/ref)


############################################
#### Compare PSD of multiple recordings ####
############################################

recordings <- c("230508_1776.wav","230508_1780.wav")

# Enter calibration constants
calib.hydrophone <- -158 # AS-1: -208; AS-1 + PA4: -158
calib.recorder <- -5.67 # lvl 1: 25.36; lvl 4: 15.28; lvl 10: -5.67
calib.correction <- 16.5 # for using the XLR port rather than the audio jack

# Sample rate
fs <- 44100

welch.df <- NULL
for(i in recordings){
  # Read wav
  signal.raw <- read.wav(i)
  
  ## Calibrate signal
  signal.calibrated <- calibrateSignal.Time(data = signal.raw[[1]],fs = fs,
                                            calib = (calib.hydrophone - calib.recorder - calib.correction))

  # Make PSD
  welch <- ezWelch(signal.calibrated, wl = (1024*6), olap = 0.5, fs = fs, windowType = 'Hann')

  # Save PSD in df
  if(exists('welch.df') && is.data.frame(get('welch.df'))){
    welch.df$PSD <- welch$PSD
  }else{
    welch.df <- data.frame('Frequency' = welch$Frequency, 'PSD' = welch$PSD)
  }
  names(welch.df)[names(welch.df) == "PSD"] <- i
}

# Transpose welch.df from short to long format
install.packages('tidyr')
require(tidyr)
welch.df.long <- gather(welch.df, file, PSD, recordings[1]:recordings[length(recordings)], 
                        factor_key=TRUE)

#install_git("https://gitlab.com/RTbecard/ezPSD.git", build_vignettes = T, force = T)
require(ezPSD)

#install.packages('ggplot2')
require(ggplot2)

# Plot
ggplot(data = welch.df.long, aes(x = welch.df.long$Frequency, 
                                 y = 10*log10(welch.df.long$PSD),
                                 color = welch.df.long$file)) +
  geom_line() +
  xlim(0,3000) +
  ylim(50, 100)+
  ylab(expression(paste("SPL (dB ref 1 ",mu,"Pa"^"2","/Hz)")))+
  xlab("Frequency (Hz)")+
  scale_color_discrete(name = "File") +
  # Use the next line to give the file another name
  #scale_color_discrete(labels = c("A", "B", "C", "D", "E")) +
  theme_bw()

