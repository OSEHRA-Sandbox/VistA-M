IBDEI338 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,51801,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51801,1,3,0)
 ;;=3^Empyema w/ Fistula
 ;;^UTILITY(U,$J,358.3,51801,1,4,0)
 ;;=4^J86.0
 ;;^UTILITY(U,$J,358.3,51801,2)
 ;;=^5008308
 ;;^UTILITY(U,$J,358.3,51802,0)
 ;;=J86.9^^230^2519^11
 ;;^UTILITY(U,$J,358.3,51802,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51802,1,3,0)
 ;;=3^Empyema w/o Fistula
 ;;^UTILITY(U,$J,358.3,51802,1,4,0)
 ;;=4^J86.9
 ;;^UTILITY(U,$J,358.3,51802,2)
 ;;=^5008309
 ;;^UTILITY(U,$J,358.3,51803,0)
 ;;=R09.02^^230^2519^13
 ;;^UTILITY(U,$J,358.3,51803,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51803,1,3,0)
 ;;=3^Hypoxemia
 ;;^UTILITY(U,$J,358.3,51803,1,4,0)
 ;;=4^R09.02
 ;;^UTILITY(U,$J,358.3,51803,2)
 ;;=^332831
 ;;^UTILITY(U,$J,358.3,51804,0)
 ;;=J98.2^^230^2519^15
 ;;^UTILITY(U,$J,358.3,51804,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51804,1,3,0)
 ;;=3^Interstitial Lung Disease
 ;;^UTILITY(U,$J,358.3,51804,1,4,0)
 ;;=4^J98.2
 ;;^UTILITY(U,$J,358.3,51804,2)
 ;;=^39734
 ;;^UTILITY(U,$J,358.3,51805,0)
 ;;=J99.^^230^2519^16
 ;;^UTILITY(U,$J,358.3,51805,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51805,1,3,0)
 ;;=3^Lung Involvement in Oth Diseases
 ;;^UTILITY(U,$J,358.3,51805,1,4,0)
 ;;=4^J99.
 ;;^UTILITY(U,$J,358.3,51805,2)
 ;;=^5008367
 ;;^UTILITY(U,$J,358.3,51806,0)
 ;;=J98.4^^230^2519^17
 ;;^UTILITY(U,$J,358.3,51806,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51806,1,3,0)
 ;;=3^Lung Nodule
 ;;^UTILITY(U,$J,358.3,51806,1,4,0)
 ;;=4^J98.4
 ;;^UTILITY(U,$J,358.3,51806,2)
 ;;=^5008362
 ;;^UTILITY(U,$J,358.3,51807,0)
 ;;=J18.9^^230^2519^18
 ;;^UTILITY(U,$J,358.3,51807,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51807,1,3,0)
 ;;=3^Pneumonia,Unspec
 ;;^UTILITY(U,$J,358.3,51807,1,4,0)
 ;;=4^J18.9
 ;;^UTILITY(U,$J,358.3,51807,2)
 ;;=^95632
 ;;^UTILITY(U,$J,358.3,51808,0)
 ;;=J84.10^^230^2519^19
 ;;^UTILITY(U,$J,358.3,51808,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51808,1,3,0)
 ;;=3^Postinflam Pulm Fibrosis
 ;;^UTILITY(U,$J,358.3,51808,1,4,0)
 ;;=4^J84.10
 ;;^UTILITY(U,$J,358.3,51808,2)
 ;;=^5008300
 ;;^UTILITY(U,$J,358.3,51809,0)
 ;;=Z01.811^^230^2519^20
 ;;^UTILITY(U,$J,358.3,51809,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51809,1,3,0)
 ;;=3^Pre-Op Respiratory Exam
 ;;^UTILITY(U,$J,358.3,51809,1,4,0)
 ;;=4^Z01.811
 ;;^UTILITY(U,$J,358.3,51809,2)
 ;;=^5062626
 ;;^UTILITY(U,$J,358.3,51810,0)
 ;;=J18.2^^230^2519^12
 ;;^UTILITY(U,$J,358.3,51810,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51810,1,3,0)
 ;;=3^Hypostatic Pneumonia,Unspec Organism
 ;;^UTILITY(U,$J,358.3,51810,1,4,0)
 ;;=4^J18.2
 ;;^UTILITY(U,$J,358.3,51810,2)
 ;;=^5008184
 ;;^UTILITY(U,$J,358.3,51811,0)
 ;;=J81.1^^230^2519^22
 ;;^UTILITY(U,$J,358.3,51811,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51811,1,3,0)
 ;;=3^Pulmonary Edema,Chronic
 ;;^UTILITY(U,$J,358.3,51811,1,4,0)
 ;;=4^J81.1
 ;;^UTILITY(U,$J,358.3,51811,2)
 ;;=^5008296
 ;;^UTILITY(U,$J,358.3,51812,0)
 ;;=J84.112^^230^2519^14
 ;;^UTILITY(U,$J,358.3,51812,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51812,1,3,0)
 ;;=3^Idiopathic Pulmonary Fibrosis
 ;;^UTILITY(U,$J,358.3,51812,1,4,0)
 ;;=4^J84.112
 ;;^UTILITY(U,$J,358.3,51812,2)
 ;;=^340534
 ;;^UTILITY(U,$J,358.3,51813,0)
 ;;=I27.0^^230^2519^21
 ;;^UTILITY(U,$J,358.3,51813,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51813,1,3,0)
 ;;=3^Primary Pulmonary Hypertension
 ;;^UTILITY(U,$J,358.3,51813,1,4,0)
 ;;=4^I27.0
 ;;^UTILITY(U,$J,358.3,51813,2)
 ;;=^265310
 ;;^UTILITY(U,$J,358.3,51814,0)
 ;;=J98.8^^230^2519^26
 ;;^UTILITY(U,$J,358.3,51814,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,51814,1,3,0)
 ;;=3^Respiratory System Disease NEC
