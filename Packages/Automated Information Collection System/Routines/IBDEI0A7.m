IBDEI0A7 ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13594,0)
 ;;=96366^^82^723^4^^^^1
 ;;^UTILITY(U,$J,358.3,13594,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13594,1,2,0)
 ;;=2^96366
 ;;^UTILITY(U,$J,358.3,13594,1,3,0)
 ;;=3^THER/PROPH/DIAG IV INF EA ADD HR
 ;;^UTILITY(U,$J,358.3,13595,0)
 ;;=96367^^82^723^5^^^^1
 ;;^UTILITY(U,$J,358.3,13595,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13595,1,2,0)
 ;;=2^96367
 ;;^UTILITY(U,$J,358.3,13595,1,3,0)
 ;;=3^TX/PROPH/DX IV INF,ADDL SEQ
 ;;^UTILITY(U,$J,358.3,13596,0)
 ;;=96374^^82^723^6^^^^1
 ;;^UTILITY(U,$J,358.3,13596,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13596,1,2,0)
 ;;=2^96374
 ;;^UTILITY(U,$J,358.3,13596,1,3,0)
 ;;=3^IV PUSH,SINGLE/INIT SUBST/DRUG
 ;;^UTILITY(U,$J,358.3,13597,0)
 ;;=96375^^82^723^7^^^^1
 ;;^UTILITY(U,$J,358.3,13597,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13597,1,2,0)
 ;;=2^96375
 ;;^UTILITY(U,$J,358.3,13597,1,3,0)
 ;;=3^IV PUSH,EA ADDL SUBST/DRUG
 ;;^UTILITY(U,$J,358.3,13598,0)
 ;;=96360^^82^723^1^^^^1
 ;;^UTILITY(U,$J,358.3,13598,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13598,1,2,0)
 ;;=2^96360
 ;;^UTILITY(U,$J,358.3,13598,1,3,0)
 ;;=3^HYDRATION IV,INIT HR
 ;;^UTILITY(U,$J,358.3,13599,0)
 ;;=96361^^82^723^2^^^^1
 ;;^UTILITY(U,$J,358.3,13599,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13599,1,2,0)
 ;;=2^96361
 ;;^UTILITY(U,$J,358.3,13599,1,3,0)
 ;;=3^HYDRATION IV,EA ADDL HR
 ;;^UTILITY(U,$J,358.3,13600,0)
 ;;=36591^^82^724^2^^^^1
 ;;^UTILITY(U,$J,358.3,13600,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13600,1,2,0)
 ;;=2^36591
 ;;^UTILITY(U,$J,358.3,13600,1,3,0)
 ;;=3^DRAW BLOOD OFF VENOUS DEVICE
 ;;^UTILITY(U,$J,358.3,13601,0)
 ;;=36592^^82^724^1^^^^1
 ;;^UTILITY(U,$J,358.3,13601,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13601,1,2,0)
 ;;=2^36592
 ;;^UTILITY(U,$J,358.3,13601,1,3,0)
 ;;=3^COLLECT BLOOD FROM PICC
 ;;^UTILITY(U,$J,358.3,13602,0)
 ;;=36569^^82^724^3^^^^1
 ;;^UTILITY(U,$J,358.3,13602,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13602,1,2,0)
 ;;=2^36569
 ;;^UTILITY(U,$J,358.3,13602,1,3,0)
 ;;=3^INSERT PICC CATH
 ;;^UTILITY(U,$J,358.3,13603,0)
 ;;=36589^^82^724^4^^^^1
 ;;^UTILITY(U,$J,358.3,13603,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13603,1,2,0)
 ;;=2^36589
 ;;^UTILITY(U,$J,358.3,13603,1,3,0)
 ;;=3^REMOVE PICC CATH
 ;;^UTILITY(U,$J,358.3,13604,0)
 ;;=99406^^82^725^1^^^^1
 ;;^UTILITY(U,$J,358.3,13604,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13604,1,2,0)
 ;;=2^99406
 ;;^UTILITY(U,$J,358.3,13604,1,3,0)
 ;;=3^BEHAV CHNG SMOKING 3-10 MIN
 ;;^UTILITY(U,$J,358.3,13605,0)
 ;;=99407^^82^725^2^^^^1
 ;;^UTILITY(U,$J,358.3,13605,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13605,1,2,0)
 ;;=2^99407
 ;;^UTILITY(U,$J,358.3,13605,1,3,0)
 ;;=3^BEHAV CHNG SMOKING > 10 MIN
 ;;^UTILITY(U,$J,358.3,13606,0)
 ;;=96401^^82^726^1^^^^1
 ;;^UTILITY(U,$J,358.3,13606,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13606,1,2,0)
 ;;=2^96401
 ;;^UTILITY(U,$J,358.3,13606,1,3,0)
 ;;=3^CHEMO ANTI-NEOPL SQ/IM
 ;;^UTILITY(U,$J,358.3,13607,0)
 ;;=96409^^82^726^2^^^^1
 ;;^UTILITY(U,$J,358.3,13607,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13607,1,2,0)
 ;;=2^96409
 ;;^UTILITY(U,$J,358.3,13607,1,3,0)
 ;;=3^CHEMO IV PUSH SNGL DRUG
 ;;^UTILITY(U,$J,358.3,13608,0)
 ;;=96411^^82^726^3^^^^1
 ;;^UTILITY(U,$J,358.3,13608,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13608,1,2,0)
 ;;=2^96411
 ;;^UTILITY(U,$J,358.3,13608,1,3,0)
 ;;=3^CHEMO IV PUSH ADDL DRUG
 ;;^UTILITY(U,$J,358.3,13609,0)
 ;;=789.00^^83^727^1
 ;;^UTILITY(U,$J,358.3,13609,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13609,1,3,0)
 ;;=3^789.00
 ;;^UTILITY(U,$J,358.3,13609,1,4,0)
 ;;=4^ABDOMINAL PAIN, UNSP
 ;;^UTILITY(U,$J,358.3,13609,2)
 ;;=^303317
 ;;^UTILITY(U,$J,358.3,13610,0)
 ;;=786.50^^83^727^2
 ;;^UTILITY(U,$J,358.3,13610,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13610,1,3,0)
 ;;=3^786.50
 ;;^UTILITY(U,$J,358.3,13610,1,4,0)
 ;;=4^CHEST PAIN
 ;;^UTILITY(U,$J,358.3,13610,2)
 ;;=^22485
 ;;^UTILITY(U,$J,358.3,13611,0)
 ;;=786.2^^83^727^5
 ;;^UTILITY(U,$J,358.3,13611,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13611,1,3,0)
 ;;=3^786.2
 ;;^UTILITY(U,$J,358.3,13611,1,4,0)
 ;;=4^COUGH
 ;;^UTILITY(U,$J,358.3,13611,2)
 ;;=^28905
 ;;^UTILITY(U,$J,358.3,13612,0)
 ;;=782.3^^83^727^6
 ;;^UTILITY(U,$J,358.3,13612,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13612,1,3,0)
 ;;=3^782.3
 ;;^UTILITY(U,$J,358.3,13612,1,4,0)
 ;;=4^EDEMA
 ;;^UTILITY(U,$J,358.3,13612,2)
 ;;=^38340
 ;;^UTILITY(U,$J,358.3,13613,0)
 ;;=784.0^^83^727^8
 ;;^UTILITY(U,$J,358.3,13613,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13613,1,3,0)
 ;;=3^784.0
 ;;^UTILITY(U,$J,358.3,13613,1,4,0)
 ;;=4^HEADACHE
 ;;^UTILITY(U,$J,358.3,13613,2)
 ;;=^54133
 ;;^UTILITY(U,$J,358.3,13614,0)
 ;;=780.52^^83^727^10
 ;;^UTILITY(U,$J,358.3,13614,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13614,1,3,0)
 ;;=3^780.52
 ;;^UTILITY(U,$J,358.3,13614,1,4,0)
 ;;=4^INSOMNIA
 ;;^UTILITY(U,$J,358.3,13614,2)
 ;;=^87662
 ;;^UTILITY(U,$J,358.3,13615,0)
 ;;=794.8^^83^727^11
 ;;^UTILITY(U,$J,358.3,13615,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13615,1,3,0)
 ;;=3^794.8
 ;;^UTILITY(U,$J,358.3,13615,1,4,0)
 ;;=4^LIVER FUNCTION STUDY-ABNORMAL
 ;;^UTILITY(U,$J,358.3,13615,2)
 ;;=^273450
 ;;^UTILITY(U,$J,358.3,13616,0)
 ;;=790.93^^83^727^15
 ;;^UTILITY(U,$J,358.3,13616,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13616,1,3,0)
 ;;=3^790.93
 ;;^UTILITY(U,$J,358.3,13616,1,4,0)
 ;;=4^PSA, ELEVATED
 ;;^UTILITY(U,$J,358.3,13616,2)
 ;;=^295772
 ;;^UTILITY(U,$J,358.3,13617,0)
 ;;=780.4^^83^727^18
 ;;^UTILITY(U,$J,358.3,13617,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13617,1,3,0)
 ;;=3^780.4
 ;;^UTILITY(U,$J,358.3,13617,1,4,0)
 ;;=4^VERTIGO/DIZZINESS
 ;;^UTILITY(U,$J,358.3,13617,2)
 ;;=^35946
 ;;^UTILITY(U,$J,358.3,13618,0)
 ;;=780.79^^83^727^12
 ;;^UTILITY(U,$J,358.3,13618,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13618,1,3,0)
 ;;=3^780.79
 ;;^UTILITY(U,$J,358.3,13618,1,4,0)
 ;;=4^MALAISE AND FATIGUE
 ;;^UTILITY(U,$J,358.3,13618,2)
 ;;=^1
 ;;^UTILITY(U,$J,358.3,13619,0)
 ;;=780.57^^83^727^16
 ;;^UTILITY(U,$J,358.3,13619,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13619,1,3,0)
 ;;=3^780.57
 ;;^UTILITY(U,$J,358.3,13619,1,4,0)
 ;;=4^SLEEP APNEA
 ;;^UTILITY(U,$J,358.3,13619,2)
 ;;=^293933
 ;;^UTILITY(U,$J,358.3,13620,0)
 ;;=783.21^^83^727^19
 ;;^UTILITY(U,$J,358.3,13620,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13620,1,3,0)
 ;;=3^783.21
 ;;^UTILITY(U,$J,358.3,13620,1,4,0)
 ;;=4^WEIGHT LOSS, ABNORMAL
 ;;^UTILITY(U,$J,358.3,13620,2)
 ;;=^322005
 ;;^UTILITY(U,$J,358.3,13621,0)
 ;;=783.22^^83^727^17
 ;;^UTILITY(U,$J,358.3,13621,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13621,1,3,0)
 ;;=3^783.22
 ;;^UTILITY(U,$J,358.3,13621,1,4,0)
 ;;=4^UNDERWEIGHT
 ;;^UTILITY(U,$J,358.3,13621,2)
 ;;=^322007
 ;;^UTILITY(U,$J,358.3,13622,0)
 ;;=564.00^^83^727^4
 ;;^UTILITY(U,$J,358.3,13622,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13622,1,3,0)
 ;;=3^564.00
 ;;^UTILITY(U,$J,358.3,13622,1,4,0)
 ;;=4^CONSTIPATION
 ;;^UTILITY(U,$J,358.3,13622,2)
 ;;=^1
 ;;^UTILITY(U,$J,358.3,13623,0)
 ;;=054.9^^83^727^9
 ;;^UTILITY(U,$J,358.3,13623,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13623,1,3,0)
 ;;=3^054.9
 ;;^UTILITY(U,$J,358.3,13623,1,4,0)
 ;;=4^HERPES SIMPLEX
 ;;^UTILITY(U,$J,358.3,13623,2)
 ;;=^56902
 ;;^UTILITY(U,$J,358.3,13624,0)
 ;;=780.60^^83^727^7
 ;;^UTILITY(U,$J,358.3,13624,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13624,1,3,0)
 ;;=3^780.60
 ;;^UTILITY(U,$J,358.3,13624,1,4,0)
 ;;=4^FEVER NOS
 ;;^UTILITY(U,$J,358.3,13624,2)
 ;;=^336764
 ;;^UTILITY(U,$J,358.3,13625,0)
 ;;=780.62^^83^727^13
 ;;^UTILITY(U,$J,358.3,13625,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13625,1,3,0)
 ;;=3^780.62
 ;;^UTILITY(U,$J,358.3,13625,1,4,0)
 ;;=4^POST-PROCEDURAL FEVER
 ;;^UTILITY(U,$J,358.3,13625,2)
 ;;=^336668
 ;;^UTILITY(U,$J,358.3,13626,0)
 ;;=780.63^^83^727^14
 ;;^UTILITY(U,$J,358.3,13626,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13626,1,3,0)
 ;;=3^780.63
 ;;^UTILITY(U,$J,358.3,13626,1,4,0)
 ;;=4^POST-VACCINATION FEVER
 ;;^UTILITY(U,$J,358.3,13626,2)
 ;;=^336669
 ;;^UTILITY(U,$J,358.3,13627,0)
 ;;=780.64^^83^727^3
 ;;^UTILITY(U,$J,358.3,13627,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13627,1,3,0)
 ;;=3^780.64
 ;;^UTILITY(U,$J,358.3,13627,1,4,0)
 ;;=4^CHILLS-W/O FEVER
 ;;^UTILITY(U,$J,358.3,13627,2)
 ;;=^336670
 ;;^UTILITY(U,$J,358.3,13628,0)
 ;;=413.9^^83^728^1
 ;;^UTILITY(U,$J,358.3,13628,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13628,1,3,0)
 ;;=3^413.9
 ;;^UTILITY(U,$J,358.3,13628,1,4,0)
 ;;=4^ANGINA PECTORIS/STABLE
 ;;^UTILITY(U,$J,358.3,13628,2)
 ;;=^87258
 ;;^UTILITY(U,$J,358.3,13629,0)
 ;;=427.31^^83^728^5
 ;;^UTILITY(U,$J,358.3,13629,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13629,1,3,0)
 ;;=3^427.31
 ;;^UTILITY(U,$J,358.3,13629,1,4,0)
 ;;=4^ATRIAL FIBRILLATION
 ;;^UTILITY(U,$J,358.3,13629,2)
 ;;=^11378
 ;;^UTILITY(U,$J,358.3,13630,0)
 ;;=427.9^^83^728^7
 ;;^UTILITY(U,$J,358.3,13630,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13630,1,3,0)
 ;;=3^427.9
 ;;^UTILITY(U,$J,358.3,13630,1,4,0)
 ;;=4^CARDIAC DYSRHYTHMIA
 ;;^UTILITY(U,$J,358.3,13630,2)
 ;;=^10166
 ;;^UTILITY(U,$J,358.3,13631,0)
 ;;=428.0^^83^728^8
 ;;^UTILITY(U,$J,358.3,13631,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13631,1,3,0)
 ;;=3^428.0
 ;;^UTILITY(U,$J,358.3,13631,1,4,0)
 ;;=4^CHF
 ;;^UTILITY(U,$J,358.3,13631,2)
 ;;=^54758
 ;;^UTILITY(U,$J,358.3,13632,0)
 ;;=414.9^^83^728^10
 ;;^UTILITY(U,$J,358.3,13632,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13632,1,3,0)
 ;;=3^414.9
 ;;^UTILITY(U,$J,358.3,13632,1,4,0)
 ;;=4^ISCHEMIC HEART DIS, CHRONIC
 ;;^UTILITY(U,$J,358.3,13632,2)
 ;;=^24401
 ;;^UTILITY(U,$J,358.3,13633,0)
 ;;=394.9^^83^728^12
 ;;^UTILITY(U,$J,358.3,13633,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13633,1,3,0)
 ;;=3^394.9
 ;;^UTILITY(U,$J,358.3,13633,1,4,0)
 ;;=4^MITRAL VALVE DISEASE
 ;;^UTILITY(U,$J,358.3,13633,2)
 ;;=^269571
 ;;^UTILITY(U,$J,358.3,13634,0)
 ;;=443.9^^83^728^13
 ;;^UTILITY(U,$J,358.3,13634,1,0)
 ;;=^358.31IA^4^2
