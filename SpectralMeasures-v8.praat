#	Read multiple acoustic measurements (duration, F1, F2, B1, *A1-A2, CoG, CPP) into arrays
#	S K Ahmed, created 06-04-2022
#	S K Ahmed, revised 07-04-2022, fixing A1-A2 measurement using an intermediary 1-to-1 LTAS (on the basis of code by Chad Vicenik, Bert Remijsen, Christian DiCanio) and adding A1-A2 modelling for normalised *A1-A2 output
#	W F L Heeren, revised 25-04-2022, adding form for formant settings, and replacing Strings object by 2nd tier in textgrid
#	G de Wit, revised 12-05-2022, adding separators between fields to facilitate import in Excel
#	S K Ahmed, revised 07-10-2022, adding options for CSV and TSV export, and adding B1 modelling for DeltaB1 output
#   S K Ahmed, revised 20-01-2023, adding CPP measurement
#   S K Ahmed, revised 29-03-2023, adding CPP average across different filters (following Hillenbrand et al. (1994)), changing Spectrogram time window (to Praat standard 30ms), adding heuristics (from Barreda 2021) for F1 and B1 errors
#	S K Ahmed, revised 28-04-2023, parabolic interpolation for A1, A2 measurement and cepstrum slice taken at t2 instead of 0.1s

# Check if a sound and textgrid are selected at the start
if numberOfSelected ("Sound") <> 1 or numberOfSelected ("TextGrid") <> 1
	exit Please select a Sound and a TextGrid first.
endif

# Formant and output settings: How many formants in what frequency range? What should the output look like?

form Settings
	comment Formant settings
	positive nFormants 5
	positive freq_range 5000
	comment Output settings
	choice Column_separator: 1
		button comma (.csv)
		button tab (.tsv)
		button pipe (.txt)
	text outputfile data
endform

if column_separator$ = "comma (.csv)"
	sep$ = ","
	ext$ = ".csv"

	elsif column_separator$ = "tab (.tsv)"
	sep$ = tab$
	ext$ = ".tsv"

	elsif column_separator$ = "pipe (.txt)"
	sep$ = "|"
	ext$ = ".txt"

	else
	exit Error with column separator.
endif

# Assigning variables to 2 selected objects:
# Sound object = relevant file
# TextGrid object = annotated for the Sound object with intervals labelled * marking vowels where measurements should be taken

idSnd = selected ("Sound")
idTG = selected ("TextGrid")
filename$ = selected$ ("TextGrid")

# Preparing output file with headers (use if not accompanied by auxiliary script)
# writeFileLine: "'outputfile$''ext$'", "Filename", sep$, "Word", sep$, "Time", sep$, "Vowel", sep$, "URvowel" sep$, "ATR", sep$, "MorphGroup", sep$, "Duration", sep$, "F1", sep$, "F2", sep$, "B1", sep$, "DeltaB1", sep$, "NormA1-A2", sep$, "CoG", sep$, "CPPNoFilt", sep$, "CPPAverage"

# Counting number of intervals marked * in TextGrid object

selectObject: idTG
nVowels = 0
nIntervals = Get number of intervals: 1
for i from 1 to nIntervals
	label$ = Get label of interval: 1, i
	if label$ = "*"
		nVowels = nVowels + 1
		vowels[nVowels] = i
	endif
endfor

# Creating a spectrogram from Sound object

selectObject: idSnd
idVowelSpecgram = To Spectrogram: 0.03, 11025, 0.005, 5, "Gaussian"

# Cycling through each marked vowel and saving acoustic measurements into arrays (and printing in the output file)

for i from 1 to nVowels
	interval = vowels[i]

	# Time measurements

	selectObject: idTG
	starttime[i] = Get start time of interval: 1, interval
	endtime[i] = Get end time of interval: 1, interval
	duration[i] = endtime[i] - starttime[i]
	t1[i] = 0.3 * duration[i] + starttime[i]
	t2[i] = 0.5 * duration[i] + starttime[i]
	t3[i] = 0.7 * duration[i] + starttime[i]

	# Taking a spectral slice at vowel midpoint (t2) and converting to LTAS (1-to-1) for A1/A2 readings

	selectObject: idVowelSpecgram
	idVowelSlice = To Spectrum (slice): t2[i]
	idVoweltas = To Ltas (1-to-1)

	# Extracting vowel window without transition effects (between t1 and t3)
	# Making Spectrum (for CoG), Formant and Cepstrum (for CPP) objects of this range

	selectObject: idSnd
	idVowelSnd = Extract part: t1[i], t3[i], "rectangular", 1, "yes"
	selectObject: idVowelSnd
	idVowelSpecRange = To Spectrum: "no"
	selectObject: idVowelSnd
	idVowelForm = To Formant (burg): 0, nFormants, freq_range, 0.025, 50
    selectObject: idVowelSnd
    idVowelSndBandPass = Filter (pass Hann band): 2500, 3500, 100
	selectObject: idVowelSnd
	idVowelSndHighPass = Filter (pass Hann band): 2500, 0, 100

    #NoFilter
	selectObject: idVowelSnd
    idCepsgramNF = To PowerCepstrogram: 60, 0.002, 5000, 50
    idCepsRangeNF = To PowerCepstrum (slice): t2[i]
    cpp_nofilt[i] = Get peak prominence: 60, 333.3, "parabolic", 0.001, 0.05, "Straight", "Robust slow"
	removeObject: idCepsgramNF, idCepsRangeNF

	#BandPass
	selectObject: idVowelSndBandPass
    idCepsgramBP = To PowerCepstrogram: 60, 0.002, 5000, 50
    idCepsRangeBP = To PowerCepstrum (slice): t2[i]
    cpp_band[i] = Get peak prominence: 60, 333.3, "parabolic", 0.001, 0.05, "Straight", "Robust slow"
	removeObject: idCepsgramBP, idCepsRangeBP

	#HighPass
	selectObject: idVowelSndHighPass
    idCepsgramHP = To PowerCepstrogram: 60, 0.002, 5000, 50
    idCepsRangeHP = To PowerCepstrum (slice): t2[i]
    cpp_high[i] = Get peak prominence: 60, 333.3, "parabolic", 0.001, 0.05, "Straight", "Robust slow"
	removeObject: idCepsgramHP, idCepsRangeHP

	cpp_avg[i] = (cpp_nofilt[i] + cpp_band[i] + cpp_high[i] ) / 3


	# Adding word from textgrid tier 2 into the words$[] array

	selectObject: idTG
	wordIntervalNumber = Get interval at time: 2, t2[i]
	words$[i] = Get label of interval: 2, wordIntervalNumber

	# Formant measurements (F1, F2 with the range t1-t3, then B1/F1mid/F2mid specifically at the midpoint t2 for A1/A2 analysis)

	selectObject: idVowelForm
	f1[i] = Get mean: 1, t1[i], t3[i], "hertz"
	f2[i] = Get mean: 2, t1[i], t3[i], "hertz"
	b1[i] = Get bandwidth at time: 1, t2[i], "hertz", "linear"

	# Formant measurements at midpoint (for A1, A2 analysis and modelling)

	f1mid = Get value at time: 1, t2[i], "hertz", "linear"
	f2mid = Get value at time: 2, t2[i], "hertz", "linear"
	f3mid = Get value at time: 3, t2[i], "hertz", "linear"

	# Error-reducing heuristics in the vein of Barreda (2021) - to avoid common formant tracking errors

	if f1[i] > 1000 or b1[i] > 500 or f1mid > 1000
		# appendInfoLine: "Barreda heuristic condition met: ", words$[i], " f1: ", f1[i], " b1: ", b1[i]

		selectObject: idVowelSnd
		idVowelFormAlt = To Formant (burg): 0, nFormants, 4900, 0.025, 50

		selectObject: idVowelFormAlt
		f1[i] = Get mean: 1, t1[i], t3[i], "hertz"
		f2[i] = Get mean: 2, t1[i], t3[i], "hertz"
		b1[i] = Get bandwidth at time: 1, t2[i], "hertz", "linear"
		f1mid = Get value at time: 1, t2[i], "hertz", "linear"
		f2mid = Get value at time: 2, t2[i], "hertz", "linear"
		f3mid = Get value at time: 3, t2[i], "hertz", "linear"

		if f1[i] > 1000 or b1[i] > 500 or f1mid > 1000
			# appendInfoLine: "Barreda heuristic still met: ", words$[i], " f1: ", f1[i], " b1: ", b1[i]

			selectObject: idVowelSnd
			idVowelFormAlt2 = To Formant (burg): 0, nFormants, 4800, 0.025, 50

			selectObject: idVowelFormAlt2
			f1[i] = Get mean: 1, t1[i], t3[i], "hertz"
			f2[i] = Get mean: 2, t1[i], t3[i], "hertz"
			b1[i] = Get bandwidth at time: 1, t2[i], "hertz", "linear"
			f1mid = Get value at time: 1, t2[i], "hertz", "linear"
			f2mid = Get value at time: 2, t2[i], "hertz", "linear"
			f3mid = Get value at time: 3, t2[i], "hertz", "linear"

			removeObject: idVowelFormAlt2
		endif
		# appendInfoLine: "New values: ", words$[i], " f1: ", f1[i], " b1: ", b1[i]
		removeObject: idVowelFormAlt
	endif

	# Modelling DeltaB1 following Fant (1972) and Starwalt (2008)

	deltab1[i] = b1[i] - (15 * (500 / f1mid) ^ 2 + 20 * (f1mid / 500) ^ 0.5 + 5 * (f1mid / 500) ^ 2)

	# Modelling A1-A2 following Fulop, Kari & Ladefoged (1998)

	modelb1 = 30
	modelb2 = 80
	modelb3 = 150
	f = f1mid
	modela1_f1 = 20 * log10 ((f1mid ^ 2 + (modelb1 / 2) ^ 2)/(sqrt((f - f1mid) ^ 2 + (modelb1 / 2) ^ 2) * sqrt((f + f1mid) ^ 2 + (modelb1 / 2) ^ 2)))
	modela1_f2 = 20 * log10 ((f2mid ^ 2 + (modelb2 / 2) ^ 2)/(sqrt((f - f2mid) ^ 2 + (modelb2 / 2) ^ 2) * sqrt((f + f2mid) ^ 2 + (modelb2 / 2) ^ 2)))
	modela1_f3 = 20 * log10 ((f3mid ^ 2 + (modelb3 / 2) ^ 2)/(sqrt((f - f3mid) ^ 2 + (modelb3 / 2) ^ 2) * sqrt((f + f3mid) ^ 2 + (modelb3 / 2) ^ 2)))
	modela1_fN = 0.72 * (f / 492) ^ 2 + 0.0033 * (f / 492) ^ 4
	modela1_glot = 20 * log10 (2 * ((f / 100) / (1 + (f / 100) ^ 2)))
	modela1 = modela1_f1 + modela1_f2 + modela1_f3 + modela1_fN + modela1_glot
	f = f2mid
	modela2_f1 = 20 * log10 ((f1mid ^ 2 + (modelb1 / 2) ^ 2)/(sqrt((f - f1mid) ^ 2 + (modelb1 / 2) ^ 2) * sqrt((f + f1mid) ^ 2 + (modelb1 / 2) ^ 2)))
	modela2_f2 = 20 * log10 ((f2mid ^ 2 + (modelb2 / 2) ^ 2)/(sqrt((f - f2mid) ^ 2 + (modelb2 / 2) ^ 2) * sqrt((f + f2mid) ^ 2 + (modelb2 / 2) ^ 2)))
	modela2_f3 = 20 * log10 ((f3mid ^ 2 + (modelb3 / 2) ^ 2)/(sqrt((f - f3mid) ^ 2 + (modelb3 / 2) ^ 2) * sqrt((f + f3mid) ^ 2 + (modelb3 / 2) ^ 2)))
	modela2_fN = 0.72 * (f / 492) ^ 2 + 0.0033 * (f / 492) ^ 4
	modela2_glot = 20 * log10 (2 * ((f / 100) / (1 + (f / 100) ^ 2)))
	modela2 = modela2_f1 + modela2_f2 + modela2_f3 + modela2_fN + modela2_glot

	modela1_a2 = modela1 - modela2

	# Formant amplitude measurements (A1, A2, A1-A2, normalised A1-A2)

	selectObject: idVoweltas
	a1[i] = Get maximum: f1mid - 150, f1mid + 150, "parabolic"
	a2[i] = Get maximum: f2mid - 150, f2mid + 150, "parabolic"
	a1_a2[i] = a1[i] - a2[i]

	norma1_a2[i] = a1_a2[i] - modela1_a2

	# Centre of gravity measurement

	selectObject: idVowelSpecRange
	cog[i] = Get centre of gravity: 2.0

	removeObject: idVowelSnd, idVowelSpecRange, idVowelSlice, idVoweltas, idVowelForm, idVowelSndBandPass, idVowelSndHighPass

	# Recording all measurements into the output file
	appendFileLine: "'outputfile$''ext$'", filename$, sep$, words$[i], sep$, t2[i], sep$, sep$, sep$, sep$, sep$, duration[i], sep$, f1[i], sep$, f2[i], sep$, b1[i], sep$, deltab1[i], sep$, norma1_a2[i], sep$, cog[i], sep$, cpp_nofilt[i], sep$, cpp_avg[i]

endfor

removeObject: idVowelSpecgram
selectObject: idSnd, idTG