IBDEI0Z9 ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,16569,1,4,0)
 ;;=4^N18.5
 ;;^UTILITY(U,$J,358.3,16569,2)
 ;;=^5015606
 ;;^UTILITY(U,$J,358.3,16570,0)
 ;;=N18.6^^67^765^8
 ;;^UTILITY(U,$J,358.3,16570,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16570,1,3,0)
 ;;=3^ESRD
 ;;^UTILITY(U,$J,358.3,16570,1,4,0)
 ;;=4^N18.6
 ;;^UTILITY(U,$J,358.3,16570,2)
 ;;=^303986
 ;;^UTILITY(U,$J,358.3,16571,0)
 ;;=N18.3^^67^765^4
 ;;^UTILITY(U,$J,358.3,16571,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16571,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 3
 ;;^UTILITY(U,$J,358.3,16571,1,4,0)
 ;;=4^N18.3
 ;;^UTILITY(U,$J,358.3,16571,2)
 ;;=^5015604
 ;;^UTILITY(U,$J,358.3,16572,0)
 ;;=N17.9^^67^765^1
 ;;^UTILITY(U,$J,358.3,16572,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16572,1,3,0)
 ;;=3^Acute Kidney Failure,Unspec
 ;;^UTILITY(U,$J,358.3,16572,1,4,0)
 ;;=4^N17.9
 ;;^UTILITY(U,$J,358.3,16572,2)
 ;;=^338532
 ;;^UTILITY(U,$J,358.3,16573,0)
 ;;=N18.1^^67^765^2
 ;;^UTILITY(U,$J,358.3,16573,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16573,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 1
 ;;^UTILITY(U,$J,358.3,16573,1,4,0)
 ;;=4^N18.1
 ;;^UTILITY(U,$J,358.3,16573,2)
 ;;=^5015602
 ;;^UTILITY(U,$J,358.3,16574,0)
 ;;=N18.2^^67^765^3
 ;;^UTILITY(U,$J,358.3,16574,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16574,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 2
 ;;^UTILITY(U,$J,358.3,16574,1,4,0)
 ;;=4^N18.2
 ;;^UTILITY(U,$J,358.3,16574,2)
 ;;=^5015603
 ;;^UTILITY(U,$J,358.3,16575,0)
 ;;=N18.9^^67^765^7
 ;;^UTILITY(U,$J,358.3,16575,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16575,1,3,0)
 ;;=3^Chronic Kidney Disease,Unspec
 ;;^UTILITY(U,$J,358.3,16575,1,4,0)
 ;;=4^N18.9
 ;;^UTILITY(U,$J,358.3,16575,2)
 ;;=^332812
 ;;^UTILITY(U,$J,358.3,16576,0)
 ;;=J44.9^^67^766^7
 ;;^UTILITY(U,$J,358.3,16576,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16576,1,3,0)
 ;;=3^COPD,Unspec
 ;;^UTILITY(U,$J,358.3,16576,1,4,0)
 ;;=4^J44.9
 ;;^UTILITY(U,$J,358.3,16576,2)
 ;;=^5008241
 ;;^UTILITY(U,$J,358.3,16577,0)
 ;;=J96.00^^67^766^1
 ;;^UTILITY(U,$J,358.3,16577,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16577,1,3,0)
 ;;=3^Acute Respiratory Failure
 ;;^UTILITY(U,$J,358.3,16577,1,4,0)
 ;;=4^J96.00
 ;;^UTILITY(U,$J,358.3,16577,2)
 ;;=^5008347
 ;;^UTILITY(U,$J,358.3,16578,0)
 ;;=J96.90^^67^766^16
 ;;^UTILITY(U,$J,358.3,16578,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16578,1,3,0)
 ;;=3^Respiratory Failure,Unspec
 ;;^UTILITY(U,$J,358.3,16578,1,4,0)
 ;;=4^J96.90
 ;;^UTILITY(U,$J,358.3,16578,2)
 ;;=^5008356
 ;;^UTILITY(U,$J,358.3,16579,0)
 ;;=J43.9^^67^766^11
 ;;^UTILITY(U,$J,358.3,16579,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16579,1,3,0)
 ;;=3^Emphysema,Unspec
 ;;^UTILITY(U,$J,358.3,16579,1,4,0)
 ;;=4^J43.9
 ;;^UTILITY(U,$J,358.3,16579,2)
 ;;=^5008238
 ;;^UTILITY(U,$J,358.3,16580,0)
 ;;=J96.20^^67^766^4
 ;;^UTILITY(U,$J,358.3,16580,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16580,1,3,0)
 ;;=3^Acute/Chr Respiratory Failure
 ;;^UTILITY(U,$J,358.3,16580,1,4,0)
 ;;=4^J96.20
 ;;^UTILITY(U,$J,358.3,16580,2)
 ;;=^5008353
 ;;^UTILITY(U,$J,358.3,16581,0)
 ;;=J69.0^^67^766^13
 ;;^UTILITY(U,$J,358.3,16581,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16581,1,3,0)
 ;;=3^Pneumonitis d/t Inhalation of Food & Vomit
 ;;^UTILITY(U,$J,358.3,16581,1,4,0)
 ;;=4^J69.0
 ;;^UTILITY(U,$J,358.3,16581,2)
 ;;=^5008288
 ;;^UTILITY(U,$J,358.3,16582,0)
 ;;=J98.9^^67^766^15
 ;;^UTILITY(U,$J,358.3,16582,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16582,1,3,0)
 ;;=3^Respiratory Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,16582,1,4,0)
 ;;=4^J98.9
 ;;^UTILITY(U,$J,358.3,16582,2)
 ;;=^5008366
 ;;^UTILITY(U,$J,358.3,16583,0)
 ;;=J91.8^^67^766^12
