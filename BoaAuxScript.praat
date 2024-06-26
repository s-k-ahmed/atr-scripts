baseVdir$ = "\BoaDataFromGerrit VHÚ\Boa-Leboale - base vowels\Tazanaba\"
verbdir$ = "\BoaDataFromGerrit VHÚ\Verbs - root and suffix vowels\Tazanaba\"

baseVwav$# = {"L-voyelles_a-pg1.wav", "L-voyelles_a-e-pg2.wav", "L-voyelles_a-e-pg2.wav", "L-voyelles_e-EE-pg3.wav", "L-voyelles_e-EE-pg3.wav", "L-voyelles_EE-i-pg4.wav", "L-voyelles_EE-i-pg4.wav", "L-voyelles_i-o-pg5.wav", "L-voyelles_i-o-pg5.wav", "L-voyelles_o-OO-pg6.wav", "L-voyelles_o-OO-pg6.wav", "L-voyelles_u-pg7,8.wav"}
baseVtg$# = {"L-voyelles_a-pg1.TextGrid",    "L-voyelles_a-e-pg2-a.TextGrid",    "L-voyelles_a-e-pg2-e.TextGrid",    "L-voyelles_e-EE-pg3-e.TextGrid",    "L-voyelles_e-EE-pg3-EE.TextGrid",    "L-voyelles_EE-i-pg4-EE.TextGrid",    "L-voyelles_EE-i-pg4-i.TextGrid",    "L-voyelles_i-o-pg5-i.TextGrid",    "L-voyelles_i-o-pg5-o.TextGrid",    "L-voyelles_o-OO-pg6-o.TextGrid",    "L-voyelles_o-OO-pg6-OO.TextGrid",    "L-voyelles_u-pg7_8.TextGrid"}
verbwav$# = {"L_pg1-futur.wav","L_pg10-caus.wav","L_pg11-caus.wav","L_pg12-caus.wav","L_pg12-negfut.wav","L_pg13-negfut.wav","L_pg14-negfut.wav","L_pg2-futur.wav","L_pg3-appl.wav","L_pg4a-appl.wav","L_pg4b-appl.wav","L_pg5-appl-subj.wav","L_pg6-subj.wav","L_pg7_subj.wav","L_pg8-recpast.wav","L_pg9-caus.wav","L_pg9-recpast.wav"}
verbtg$# = {"L_pg1-futur.TextGrid","L_pg10-caus.TextGrid","L_pg11-caus.TextGrid","L_pg12-caus.TextGrid","L_pg12-negfut.TextGrid","L_pg13-negfut.TextGrid","L_pg14-negfut.TextGrid","L_pg2-futur.TextGrid","L_pg3-appl.TextGrid","L_pg4a-appl.TextGrid","L_pg4b-appl.TextGrid","L_pg5-appl-subj.TextGrid","L_pg6-subj.TextGrid","L_pg7_subj.TextGrid","L_pg8-recpast.TextGrid","L_pg9-caus.TextGrid","L_pg9-recpast.TextGrid"}

version$ = "-v8"

writeFileLine: "BoaData'version$'.csv", "filename", ",", "word", ",", "time", ",", "vowel", ",", "urvowel", ",", "atr", ",", "morphgroup", ",", "duration", ",", "f1", ",", "f2", ",", "b1", ",", "deltab1", ",", "norma1a2", ",", "cog", ",", "cppnofilt", ",", "cppavg"

# Base Vowels

for bvtg from 1 to size(baseVtg$#)
    idTG = Read from file: baseVdir$ + baseVtg$#[bvtg]
    idSnd = Read from file: baseVdir$ + baseVwav$#[bvtg]

    selectObject: idTG, idSnd

    runScript: "SpectralMeasures'version$'.praat", 5, 5000, "comma (.csv)", "BoaData'version$'"

    removeObject: idTG, idSnd
endfor

# Verbs

for vbtg from 1 to size(verbtg$#)
    idTG = Read from file: verbdir$ + verbtg$#[vbtg]
    idSnd = Read from file: verbdir$ + verbwav$#[vbtg]

    selectObject: idTG, idSnd

    runScript: "SpectralMeasures'version$'.praat", 5, 5000, "comma (.csv)", "BoaData'version$'"

    removeObject: idTG, idSnd
endfor