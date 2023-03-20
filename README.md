---
output:
  pdf_document: default
  html_document: default
---
# SWALE
Simultaneous Waveform and Amplitude Estimation (SWALE)

## Example analysis

First simulate some data (using simulateEEGsignal). Signal at 100 ms, 15 ms width (200 ms total), 20 trials, no latency shift in peak).
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

Now set up the swale data object:

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

Set up the basis functions to be used to describe the waveform (18-order polynomial)
```r{}

#make basis function object
basis = makePoly(6,200)

```
Make the internal object that swale uses for analysis
```r{}
#make internal swale object
swaledat.int = new('swale.internal',eeg.data=data,basis=basis)

```

Make the swale control object, to control starting-points, iterations etc.
```r{}
#make new control object
control.one = new('control')

#set up
control.one@start.value = makeStart(swaledat.int)
control.one@iter.limit = 1000

#peak window (in samples)
control.one@peak.windows = list(c(50,150))
```

Use 'iterate' to estimate waveform and amp/lat. Use calcSolution to summarize output and plot

```r{}
swaledat.one = iterate(swaledat.int,control.one)
sol.one = calcSolution(swaledat.one,summarize=T)
plot(sol.one)
plotSingleTrials(sol.one)
```


