IBDEI0JZ ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,20134,1,3,0)
 ;;=3^Enterovirus Cause of Disease
 ;;^UTILITY(U,$J,358.3,20134,1,4,0)
 ;;=4^B97.10
 ;;^UTILITY(U,$J,358.3,20134,2)
 ;;=^5000861
 ;;^UTILITY(U,$J,358.3,20135,0)
 ;;=B34.9^^86^996^90
 ;;^UTILITY(U,$J,358.3,20135,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20135,1,3,0)
 ;;=3^Viral Infection,Unspec
 ;;^UTILITY(U,$J,358.3,20135,1,4,0)
 ;;=4^B34.9
 ;;^UTILITY(U,$J,358.3,20135,2)
 ;;=^5000603
 ;;^UTILITY(U,$J,358.3,20136,0)
 ;;=A69.20^^86^996^50
 ;;^UTILITY(U,$J,358.3,20136,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20136,1,3,0)
 ;;=3^Lyme Disease,Unspec
 ;;^UTILITY(U,$J,358.3,20136,1,4,0)
 ;;=4^A69.20
 ;;^UTILITY(U,$J,358.3,20136,2)
 ;;=^5000375
 ;;^UTILITY(U,$J,358.3,20137,0)
 ;;=A69.22^^86^996^52
 ;;^UTILITY(U,$J,358.3,20137,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20137,1,3,0)
 ;;=3^Neurologic Disorders d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,20137,1,4,0)
 ;;=4^A69.22
 ;;^UTILITY(U,$J,358.3,20137,2)
 ;;=^5000377
 ;;^UTILITY(U,$J,358.3,20138,0)
 ;;=A69.21^^86^996^51
 ;;^UTILITY(U,$J,358.3,20138,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20138,1,3,0)
 ;;=3^Meningitis d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,20138,1,4,0)
 ;;=4^A69.21
 ;;^UTILITY(U,$J,358.3,20138,2)
 ;;=^5000376
 ;;^UTILITY(U,$J,358.3,20139,0)
 ;;=A69.29^^86^996^18
 ;;^UTILITY(U,$J,358.3,20139,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20139,1,3,0)
 ;;=3^Conditions d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,20139,1,4,0)
 ;;=4^A69.29
 ;;^UTILITY(U,$J,358.3,20139,2)
 ;;=^5000379
 ;;^UTILITY(U,$J,358.3,20140,0)
 ;;=A69.23^^86^996^5
 ;;^UTILITY(U,$J,358.3,20140,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20140,1,3,0)
 ;;=3^Arthritis d/t Lyme Disease
 ;;^UTILITY(U,$J,358.3,20140,1,4,0)
 ;;=4^A69.23
 ;;^UTILITY(U,$J,358.3,20140,2)
 ;;=^5000378
 ;;^UTILITY(U,$J,358.3,20141,0)
 ;;=A51.0^^86^996^29
 ;;^UTILITY(U,$J,358.3,20141,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20141,1,3,0)
 ;;=3^Genital Syphilis,Primary
 ;;^UTILITY(U,$J,358.3,20141,1,4,0)
 ;;=4^A51.0
 ;;^UTILITY(U,$J,358.3,20141,2)
 ;;=^5000272
 ;;^UTILITY(U,$J,358.3,20142,0)
 ;;=A52.3^^86^996^54
 ;;^UTILITY(U,$J,358.3,20142,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20142,1,3,0)
 ;;=3^Neurosyphilis,Unspec
 ;;^UTILITY(U,$J,358.3,20142,1,4,0)
 ;;=4^A52.3
 ;;^UTILITY(U,$J,358.3,20142,2)
 ;;=^5000298
 ;;^UTILITY(U,$J,358.3,20143,0)
 ;;=A52.10^^86^996^53
 ;;^UTILITY(U,$J,358.3,20143,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20143,1,3,0)
 ;;=3^Neurosyphilis Symptomatic,Unspec
 ;;^UTILITY(U,$J,358.3,20143,1,4,0)
 ;;=4^A52.10
 ;;^UTILITY(U,$J,358.3,20143,2)
 ;;=^5000291
 ;;^UTILITY(U,$J,358.3,20144,0)
 ;;=A52.9^^86^996^49
 ;;^UTILITY(U,$J,358.3,20144,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20144,1,3,0)
 ;;=3^Late Syphilis,Unspec
 ;;^UTILITY(U,$J,358.3,20144,1,4,0)
 ;;=4^A52.9
 ;;^UTILITY(U,$J,358.3,20144,2)
 ;;=^5000308
 ;;^UTILITY(U,$J,358.3,20145,0)
 ;;=A53.9^^86^996^79
 ;;^UTILITY(U,$J,358.3,20145,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20145,1,3,0)
 ;;=3^Syphilis,Unspec
 ;;^UTILITY(U,$J,358.3,20145,1,4,0)
 ;;=4^A53.9
 ;;^UTILITY(U,$J,358.3,20145,2)
 ;;=^5000310
 ;;^UTILITY(U,$J,358.3,20146,0)
 ;;=B37.3^^86^996^17
 ;;^UTILITY(U,$J,358.3,20146,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20146,1,3,0)
 ;;=3^Candidiasis Vulva/Vagina
 ;;^UTILITY(U,$J,358.3,20146,1,4,0)
 ;;=4^B37.3
 ;;^UTILITY(U,$J,358.3,20146,2)
 ;;=^5000615
 ;;^UTILITY(U,$J,358.3,20147,0)
 ;;=B58.9^^86^996^82
 ;;^UTILITY(U,$J,358.3,20147,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20147,1,3,0)
 ;;=3^Toxoplasmosis,Unspec
 ;;^UTILITY(U,$J,358.3,20147,1,4,0)
 ;;=4^B58.9
 ;;^UTILITY(U,$J,358.3,20147,2)
 ;;=^5000733
 ;;^UTILITY(U,$J,358.3,20148,0)
 ;;=A59.01^^86^996^83
 ;;^UTILITY(U,$J,358.3,20148,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20148,1,3,0)
 ;;=3^Trichomonal Vulvovaginitis
 ;;^UTILITY(U,$J,358.3,20148,1,4,0)
 ;;=4^A59.01
 ;;^UTILITY(U,$J,358.3,20148,2)
 ;;=^121763
 ;;^UTILITY(U,$J,358.3,20149,0)
 ;;=B59.^^86^996^66
 ;;^UTILITY(U,$J,358.3,20149,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20149,1,3,0)
 ;;=3^Pneumocystosis
 ;;^UTILITY(U,$J,358.3,20149,1,4,0)
 ;;=4^B59.
 ;;^UTILITY(U,$J,358.3,20149,2)
 ;;=^5000734
 ;;^UTILITY(U,$J,358.3,20150,0)
 ;;=H83.09^^86^996^48
 ;;^UTILITY(U,$J,358.3,20150,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20150,1,3,0)
 ;;=3^Labyrinthitis,Unspec Ear
 ;;^UTILITY(U,$J,358.3,20150,1,4,0)
 ;;=4^H83.09
 ;;^UTILITY(U,$J,358.3,20150,2)
 ;;=^5006897
 ;;^UTILITY(U,$J,358.3,20151,0)
 ;;=H83.01^^86^996^47
 ;;^UTILITY(U,$J,358.3,20151,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20151,1,3,0)
 ;;=3^Labyrinthitis,Right Ear
 ;;^UTILITY(U,$J,358.3,20151,1,4,0)
 ;;=4^H83.01
 ;;^UTILITY(U,$J,358.3,20151,2)
 ;;=^5006894
 ;;^UTILITY(U,$J,358.3,20152,0)
 ;;=H83.03^^86^996^45
 ;;^UTILITY(U,$J,358.3,20152,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20152,1,3,0)
 ;;=3^Labyrinthitis,Bilateral
 ;;^UTILITY(U,$J,358.3,20152,1,4,0)
 ;;=4^H83.03
 ;;^UTILITY(U,$J,358.3,20152,2)
 ;;=^5006896
 ;;^UTILITY(U,$J,358.3,20153,0)
 ;;=H83.02^^86^996^46
 ;;^UTILITY(U,$J,358.3,20153,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20153,1,3,0)
 ;;=3^Labyrinthitis,Left Ear
 ;;^UTILITY(U,$J,358.3,20153,1,4,0)
 ;;=4^H83.02
 ;;^UTILITY(U,$J,358.3,20153,2)
 ;;=^5006895
 ;;^UTILITY(U,$J,358.3,20154,0)
 ;;=J01.91^^86^996^76
 ;;^UTILITY(U,$J,358.3,20154,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20154,1,3,0)
 ;;=3^Sinusitis,Acute Recurrent Unspec
 ;;^UTILITY(U,$J,358.3,20154,1,4,0)
 ;;=4^J01.91
 ;;^UTILITY(U,$J,358.3,20154,2)
 ;;=^5008128
 ;;^UTILITY(U,$J,358.3,20155,0)
 ;;=J03.90^^86^996^81
 ;;^UTILITY(U,$J,358.3,20155,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20155,1,3,0)
 ;;=3^Tonsillitis,Acute Unspec
 ;;^UTILITY(U,$J,358.3,20155,1,4,0)
 ;;=4^J03.90
 ;;^UTILITY(U,$J,358.3,20155,2)
 ;;=^5008135
 ;;^UTILITY(U,$J,358.3,20156,0)
 ;;=J01.90^^86^996^77
 ;;^UTILITY(U,$J,358.3,20156,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20156,1,3,0)
 ;;=3^Sinusitis,Acute Unspec
 ;;^UTILITY(U,$J,358.3,20156,1,4,0)
 ;;=4^J01.90
 ;;^UTILITY(U,$J,358.3,20156,2)
 ;;=^5008127
 ;;^UTILITY(U,$J,358.3,20157,0)
 ;;=J20.1^^86^996^10
 ;;^UTILITY(U,$J,358.3,20157,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20157,1,3,0)
 ;;=3^Bronchitis,Acute d/t Hemophilus Influenza
 ;;^UTILITY(U,$J,358.3,20157,1,4,0)
 ;;=4^J20.1
 ;;^UTILITY(U,$J,358.3,20157,2)
 ;;=^5008187
 ;;^UTILITY(U,$J,358.3,20158,0)
 ;;=J20.0^^86^996^11
 ;;^UTILITY(U,$J,358.3,20158,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20158,1,3,0)
 ;;=3^Bronchitis,Acute d/t Mycoplasma Pneumonia
 ;;^UTILITY(U,$J,358.3,20158,1,4,0)
 ;;=4^J20.0
 ;;^UTILITY(U,$J,358.3,20158,2)
 ;;=^5008186
 ;;^UTILITY(U,$J,358.3,20159,0)
 ;;=J20.2^^86^996^6
 ;;^UTILITY(U,$J,358.3,20159,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20159,1,3,0)
 ;;=3^Bonchitis,Acute d/t Streptococcus
 ;;^UTILITY(U,$J,358.3,20159,1,4,0)
 ;;=4^J20.2
 ;;^UTILITY(U,$J,358.3,20159,2)
 ;;=^5008188
 ;;^UTILITY(U,$J,358.3,20160,0)
 ;;=J20.4^^86^996^13
 ;;^UTILITY(U,$J,358.3,20160,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20160,1,3,0)
 ;;=3^Bronchitis,Acute d/t Parainfluenza Virus
 ;;^UTILITY(U,$J,358.3,20160,1,4,0)
 ;;=4^J20.4
 ;;^UTILITY(U,$J,358.3,20160,2)
 ;;=^5008190
 ;;^UTILITY(U,$J,358.3,20161,0)
 ;;=J20.3^^86^996^8
 ;;^UTILITY(U,$J,358.3,20161,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20161,1,3,0)
 ;;=3^Bronchitis,Acute d/t Coxsackievirus
 ;;^UTILITY(U,$J,358.3,20161,1,4,0)
 ;;=4^J20.3
 ;;^UTILITY(U,$J,358.3,20161,2)
 ;;=^5008189
 ;;^UTILITY(U,$J,358.3,20162,0)
 ;;=J20.9^^86^996^7
 ;;^UTILITY(U,$J,358.3,20162,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,20162,1,3,0)
 ;;=3^Bronchitis,Acute Unspec
 ;;^UTILITY(U,$J,358.3,20162,1,4,0)
 ;;=4^J20.9
 ;;^UTILITY(U,$J,358.3,20162,2)
 ;;=^5008195
 ;;^UTILITY(U,$J,358.3,20163,0)
 ;;=J20.8^^86^996^12
 ;;^UTILITY(U,$J,358.3,20163,1,0)
 ;;=^358.31IA^4^2
