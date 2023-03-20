
# SWALE
Simultaneous Waveform and Amplitude Estimation (SWALE)

## Example analysis

First simulate some data (using simulateEEGsignal). Signal at 100 ms, 15 ms width (200 ms total), 30 trials, small latency shift in peak (mean 0, sd 10, truncated at -20,20), small amplitude shifts (mean 1, sd 0.5, truncated -2,2), signal-to-noise (white) = 5.
```r{}
simdat <- swale::simulateEEGsignal(signal = 100, 
                                   samples = 200,
                                   trials = 30,
                                   snr = 5,
                                   amp.mean = 1,
                                   amp.sd = 0.5,
                                   amp.range = c(-2,2),
                                   lat.mean = 0,
                                   lat.sd = 10, 
                                   lat.range = c(-20,20))
```

Now set up the swale data object. This includes the trial x time data matrix and the (linear) detrended data matrix, and the sampling rate. Channel and Condition slots are for display purposes.

```r{}

#make new data object
data = new('eeg.data')
data@data = simdat$data
data@trend = detrend(simdat$data)$trend
data@trials = nrow(data@data)
data@samples = ncol(data@data)
data@sampRate = 512
data@channel = 'Fz'
data@condition = 'P100'

eeg.plot(simdat$data)

```

Set up the basis functions to be used to describe the waveform (6-order polynomial). This can be any basis function matrix. Default is a 6-order polynomial.

```r{}

#make basis function object (#number of basis functions by samples)
basis = makePoly(6,200)

```
Make the internal object that swale uses for analysis; it combines the data object and basis object.
```r{}
#make internal swale object
swaledat.int = new('swale.internal',eeg.data=data,basis=basis)

```

Make the swale control object, to control starting-points, iterations etc. MakeStarts defines starting values for the iteration procedure. Peak.windows defines the window in which swale searches for the maximum value (in samples).
```r{}
#make new control object
control.one = new('control')

#set up
control.one@start.value = makeStart(swaledat.int)
control.one@iter.limit = 1000

#peak window (in samples)
control.one@peak.windows = list(c(50,150))
```

Use 'iterate' to estimate waveform and amplitude / latency. Use calcSolution to summarize output and plot. PlotSingleTrials will plot the single-trial data with the swale model and the peak (blue dot).

```r{}
swaledat.one = iterate(swaledat.int,control.one)
sol.one = calcSolution(swaledat.one,summarize=T)
plot(sol.one)
plotSingleTrials(sol.one)
```


