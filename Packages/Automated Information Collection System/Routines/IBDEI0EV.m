IBDEI0EV ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,14869,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14869,1,3,0)
 ;;=3^Viral Agent Cause of Disease
 ;;^UTILITY(U,$J,358.3,14869,1,4,0)
 ;;=4^B97.89
 ;;^UTILITY(U,$J,358.3,14869,2)
 ;;=^5000879
 ;;^UTILITY(U,$J,358.3,14870,0)
 ;;=B97.10^^61^742^21
 ;;^UTILITY(U,$J,358.3,14870,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14870,1,3,0)
 ;;=3^Enterovirus Cause of Disease
 ;;^UTILITY(U,$J,358.3,14870,1,4,0)
 ;;=4^B97.10
 ;;^UTILITY(U,$J,358.3,14870,2)
 ;;=^5000861
 ;;^UTILITY(U,$J,358.3,14871,0)
 ;;=B34.9^^61^742^90
 ;;^UTILITY(U,$J,358.3,14871,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14871,1,3,0)
 ;;=3^Viral Infection,Unspec
 ;;^UTILITY(U,$J,358.3,14871,1,4,0)
 ;;=4^B34.9
 ;;^UTILITY(U,$J,358.3,14871,2)
 ;;=^5000603
 ;;^UTILITY(U,$J,358.3,14872,0)
 ;;=A69.20^^61^742^50
 ;;^UTILITY(U,$J,358.3,14872,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14872,1,3,0)
 ;;=3^Lyme Disease,Unspec
 ;;^UTILITY(U,$J,358.3,14872,1,4,0)
 ;;=4^A69.20
 ;;^UTILITY(U,$J,358.3,14872,2)
 ;;=^5000375
 ;;^UTILITY(U,$J,358.3,14873,0)
 ;;=A69.22^^61^742^52
 ;;^UTILITY(U,$J,358.3,14873,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14873,1,3,0)
 ;;=3^Neurologic Disorders d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,14873,1,4,0)
 ;;=4^A69.22
 ;;^UTILITY(U,$J,358.3,14873,2)
 ;;=^5000377
 ;;^UTILITY(U,$J,358.3,14874,0)
 ;;=A69.21^^61^742^51
 ;;^UTILITY(U,$J,358.3,14874,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14874,1,3,0)
 ;;=3^Meningitis d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,14874,1,4,0)
 ;;=4^A69.21
 ;;^UTILITY(U,$J,358.3,14874,2)
 ;;=^5000376
 ;;^UTILITY(U,$J,358.3,14875,0)
 ;;=A69.29^^61^742^18
 ;;^UTILITY(U,$J,358.3,14875,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14875,1,3,0)
 ;;=3^Conditions d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,14875,1,4,0)
 ;;=4^A69.29
 ;;^UTILITY(U,$J,358.3,14875,2)
 ;;=^5000379
 ;;^UTILITY(U,$J,358.3,14876,0)
 ;;=A69.23^^61^742^5
 ;;^UTILITY(U,$J,358.3,14876,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14876,1,3,0)
 ;;=3^Arthritis d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,14876,1,4,0)
 ;;=4^A69.23
 ;;^UTILITY(U,$J,358.3,14876,2)
 ;;=^5000378
 ;;^UTILITY(U,$J,358.3,14877,0)
 ;;=A51.0^^61^742^29
 ;;^UTILITY(U,$J,358.3,14877,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14877,1,3,0)
 ;;=3^Genital Syphilis,Primary
 ;;^UTILITY(U,$J,358.3,14877,1,4,0)
 ;;=4^A51.0
 ;;^UTILITY(U,$J,358.3,14877,2)
 ;;=^5000272
 ;;^UTILITY(U,$J,358.3,14878,0)
 ;;=A52.3^^61^742^54
 ;;^UTILITY(U,$J,358.3,14878,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14878,1,3,0)
 ;;=3^Neurosyphilis,Unspec
 ;;^UTILITY(U,$J,358.3,14878,1,4,0)
 ;;=4^A52.3
 ;;^UTILITY(U,$J,358.3,14878,2)
 ;;=^5000298
 ;;^UTILITY(U,$J,358.3,14879,0)
 ;;=A52.10^^61^742^53
 ;;^UTILITY(U,$J,358.3,14879,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14879,1,3,0)
 ;;=3^Neurosyphilis Symptomatic,Unspec
 ;;^UTILITY(U,$J,358.3,14879,1,4,0)
 ;;=4^A52.10
 ;;^UTILITY(U,$J,358.3,14879,2)
 ;;=^5000291
 ;;^UTILITY(U,$J,358.3,14880,0)
 ;;=A52.9^^61^742^49
 ;;^UTILITY(U,$J,358.3,14880,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14880,1,3,0)
 ;;=3^Late Syphilis,Unspec
 ;;^UTILITY(U,$J,358.3,14880,1,4,0)
 ;;=4^A52.9
 ;;^UTILITY(U,$J,358.3,14880,2)
 ;;=^5000308
 ;;^UTILITY(U,$J,358.3,14881,0)
 ;;=A53.9^^61^742^79
 ;;^UTILITY(U,$J,358.3,14881,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14881,1,3,0)
 ;;=3^Syphilis,Unspec
 ;;^UTILITY(U,$J,358.3,14881,1,4,0)
 ;;=4^A53.9
 ;;^UTILITY(U,$J,358.3,14881,2)
 ;;=^5000310
 ;;^UTILITY(U,$J,358.3,14882,0)
 ;;=B37.3^^61^742^17
 ;;^UTILITY(U,$J,358.3,14882,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14882,1,3,0)
 ;;=3^Candidiasis Vulva/Vagina
 ;;^UTILITY(U,$J,358.3,14882,1,4,0)
 ;;=4^B37.3
 ;;^UTILITY(U,$J,358.3,14882,2)
 ;;=^5000615
 ;;^UTILITY(U,$J,358.3,14883,0)
 ;;=B58.9^^61^742^82
 ;;^UTILITY(U,$J,358.3,14883,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14883,1,3,0)
 ;;=3^Toxoplasmosis,Unspec
 ;;^UTILITY(U,$J,358.3,14883,1,4,0)
 ;;=4^B58.9
 ;;^UTILITY(U,$J,358.3,14883,2)
 ;;=^5000733
 ;;^UTILITY(U,$J,358.3,14884,0)
 ;;=A59.01^^61^742^83
 ;;^UTILITY(U,$J,358.3,14884,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14884,1,3,0)
 ;;=3^Trichomonal Vulvovaginitis
 ;;^UTILITY(U,$J,358.3,14884,1,4,0)
 ;;=4^A59.01
 ;;^UTILITY(U,$J,358.3,14884,2)
 ;;=^121763
 ;;^UTILITY(U,$J,358.3,14885,0)
 ;;=B59.^^61^742^66
 ;;^UTILITY(U,$J,358.3,14885,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14885,1,3,0)
 ;;=3^Pneumocystosis
 ;;^UTILITY(U,$J,358.3,14885,1,4,0)
 ;;=4^B59.
 ;;^UTILITY(U,$J,358.3,14885,2)
 ;;=^5000734
 ;;^UTILITY(U,$J,358.3,14886,0)
 ;;=H83.09^^61^742^48
 ;;^UTILITY(U,$J,358.3,14886,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14886,1,3,0)
 ;;=3^Labyrinthitis,Unspec Ear
 ;;^UTILITY(U,$J,358.3,14886,1,4,0)
 ;;=4^H83.09
 ;;^UTILITY(U,$J,358.3,14886,2)
 ;;=^5006897
 ;;^UTILITY(U,$J,358.3,14887,0)
 ;;=H83.01^^61^742^47
 ;;^UTILITY(U,$J,358.3,14887,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14887,1,3,0)
 ;;=3^Labyrinthitis,Right Ear
 ;;^UTILITY(U,$J,358.3,14887,1,4,0)
 ;;=4^H83.01
 ;;^UTILITY(U,$J,358.3,14887,2)
 ;;=^5006894
 ;;^UTILITY(U,$J,358.3,14888,0)
 ;;=H83.03^^61^742^45
 ;;^UTILITY(U,$J,358.3,14888,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14888,1,3,0)
 ;;=3^Labyrinthitis,Bilateral
 ;;^UTILITY(U,$J,358.3,14888,1,4,0)
 ;;=4^H83.03
 ;;^UTILITY(U,$J,358.3,14888,2)
 ;;=^5006896
 ;;^UTILITY(U,$J,358.3,14889,0)
 ;;=H83.02^^61^742^46
 ;;^UTILITY(U,$J,358.3,14889,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14889,1,3,0)
 ;;=3^Labyrinthitis,Left Ear
 ;;^UTILITY(U,$J,358.3,14889,1,4,0)
 ;;=4^H83.02
 ;;^UTILITY(U,$J,358.3,14889,2)
 ;;=^5006895
 ;;^UTILITY(U,$J,358.3,14890,0)
 ;;=J01.91^^61^742^76
 ;;^UTILITY(U,$J,358.3,14890,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14890,1,3,0)
 ;;=3^Sinusitis,Acute Recurrent Unspec
 ;;^UTILITY(U,$J,358.3,14890,1,4,0)
 ;;=4^J01.91
 ;;^UTILITY(U,$J,358.3,14890,2)
 ;;=^5008128
 ;;^UTILITY(U,$J,358.3,14891,0)
 ;;=J03.90^^61^742^81
 ;;^UTILITY(U,$J,358.3,14891,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14891,1,3,0)
 ;;=3^Tonsillitis,Acute Unspec
 ;;^UTILITY(U,$J,358.3,14891,1,4,0)
 ;;=4^J03.90
 ;;^UTILITY(U,$J,358.3,14891,2)
 ;;=^5008135
 ;;^UTILITY(U,$J,358.3,14892,0)
 ;;=J01.90^^61^742^77
 ;;^UTILITY(U,$J,358.3,14892,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14892,1,3,0)
 ;;=3^Sinusitis,Acute Unspec
 ;;^UTILITY(U,$J,358.3,14892,1,4,0)
 ;;=4^J01.90
 ;;^UTILITY(U,$J,358.3,14892,2)
 ;;=^5008127
 ;;^UTILITY(U,$J,358.3,14893,0)
 ;;=J20.1^^61^742^10
 ;;^UTILITY(U,$J,358.3,14893,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14893,1,3,0)
 ;;=3^Bronchitis,Acute d/t Hemophilus Influenza
 ;;^UTILITY(U,$J,358.3,14893,1,4,0)
 ;;=4^J20.1
 ;;^UTILITY(U,$J,358.3,14893,2)
 ;;=^5008187
 ;;^UTILITY(U,$J,358.3,14894,0)
 ;;=J20.0^^61^742^11
 ;;^UTILITY(U,$J,358.3,14894,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14894,1,3,0)
 ;;=3^Bronchitis,Acute d/t Mycoplasma Pneumonia
 ;;^UTILITY(U,$J,358.3,14894,1,4,0)
 ;;=4^J20.0
 ;;^UTILITY(U,$J,358.3,14894,2)
 ;;=^5008186
 ;;^UTILITY(U,$J,358.3,14895,0)
 ;;=J20.2^^61^742^6
 ;;^UTILITY(U,$J,358.3,14895,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14895,1,3,0)
 ;;=3^Bonchitis,Acute d/t Streptococcus
 ;;^UTILITY(U,$J,358.3,14895,1,4,0)
 ;;=4^J20.2
 ;;^UTILITY(U,$J,358.3,14895,2)
 ;;=^5008188
 ;;^UTILITY(U,$J,358.3,14896,0)
 ;;=J20.4^^61^742^13
 ;;^UTILITY(U,$J,358.3,14896,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14896,1,3,0)
 ;;=3^Bronchitis,Acute d/t Parainfluenza Virus
 ;;^UTILITY(U,$J,358.3,14896,1,4,0)
 ;;=4^J20.4
 ;;^UTILITY(U,$J,358.3,14896,2)
 ;;=^5008190
 ;;^UTILITY(U,$J,358.3,14897,0)
 ;;=J20.3^^61^742^8
 ;;^UTILITY(U,$J,358.3,14897,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,14897,1,3,0)
 ;;=3^Bronchitis,Acute d/t Coxsackievirus
 ;;^UTILITY(U,$J,358.3,14897,1,4,0)
 ;;=4^J20.3
 ;;^UTILITY(U,$J,358.3,14897,2)
 ;;=^5008189
