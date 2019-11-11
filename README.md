# Materials, Data, & Code

For Rhodes, Abbene, Meierhofer, & Naveh-Benjamin: "Age differences in the precision of memory at short and long delays"

## task

Contains psychopy scripts for the two experiments along with the images used (from Brady et al., 2008). Note that the way in which data was written for each trial changed between experiments 1 and 2, so the files for the second experiment also include study events.

## data

Contains recall data from both experiments for each participant. The columns are:

- `block`: first or second blocks of trials
- `lag`: number of events between study and test
- `studiedX`: presented X coordinate
- `studiedY`: presented Y coordinate
- `image`: the object presented on this event
- `recalledX`: recalled X coordinate
- `recalledY`: recalled Y coordinate
- `recallError`: euclidean distance between presented and recalled
- `recallRT`: time taken to click on recalled location (in seconds)
- `conf`: confidence rating 0-5 [only experiment 1]
- `confRT`: time taken to rate confidence (in seconds) [only experiment 1]
- `interEventInt`: interval following study and test events [only experiment 2]
- `eventType`: study or test [only experiment 2]
- `lure`: the non-studied image presented in the 2AFC task [only experiment 2]
- `targetLeft`: was the studied image on the left of the screen (1 = yes, 0 = no) [only experiment 2]
- `respLeft`: did the participant choose the image on the left (1 = yes, 0 = no) [only experiment 2]
- `tafcACC`: accuracy of the 2AFC decision [only experiment 2]
- `tafcRT`: time taken to make the 2AFC decision [only experiment 2]

## analysis

Contains `R` scripts for reading in data and performing the analyses reported in the manuscript.

- `read_exp1.R` & `read_exp2.R`: read and combines the individual data files. Excludes participants with large amounts of missing data. No need to run as resulting .csv files are already in `data`
- `brm-error.R`: analyses of recall error and confidence ratings for both experiments
- `categories.R`: exploratory analyses looking at influence of categorical representations in location recall
- `stan-model`: folder containing code for mixture model
    - `mixture-2D.stan`: mixture model `stan` code
    - `fit-model-e1.R` & `fit-model-e2.R` & `fit-model-both.R`: use the `rstan` package to fit the model to the data from the experiments separately and combined
