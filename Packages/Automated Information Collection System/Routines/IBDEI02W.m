IBDEI02W ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,593,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,593,1,3,0)
 ;;=3^Anxiety Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,593,1,4,0)
 ;;=4^F41.9
 ;;^UTILITY(U,$J,358.3,593,2)
 ;;=^5003567
 ;;^UTILITY(U,$J,358.3,594,0)
 ;;=F43.10^^6^71^8
 ;;^UTILITY(U,$J,358.3,594,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,594,1,3,0)
 ;;=3^PTSD,Unspec
 ;;^UTILITY(U,$J,358.3,594,1,4,0)
 ;;=4^F43.10
 ;;^UTILITY(U,$J,358.3,594,2)
 ;;=^5003570
 ;;^UTILITY(U,$J,358.3,595,0)
 ;;=F43.12^^6^71^7
 ;;^UTILITY(U,$J,358.3,595,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,595,1,3,0)
 ;;=3^PTSD,Chronic
 ;;^UTILITY(U,$J,358.3,595,1,4,0)
 ;;=4^F43.12
 ;;^UTILITY(U,$J,358.3,595,2)
 ;;=^5003572
 ;;^UTILITY(U,$J,358.3,596,0)
 ;;=C34.90^^6^72^1
 ;;^UTILITY(U,$J,358.3,596,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,596,1,3,0)
 ;;=3^Malig Neop Bronchus/Lung Unspec Part
 ;;^UTILITY(U,$J,358.3,596,1,4,0)
 ;;=4^C34.90
 ;;^UTILITY(U,$J,358.3,596,2)
 ;;=^5000966
 ;;^UTILITY(U,$J,358.3,597,0)
 ;;=G20.^^6^73^12
 ;;^UTILITY(U,$J,358.3,597,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,597,1,3,0)
 ;;=3^Parkinson's Disease
 ;;^UTILITY(U,$J,358.3,597,1,4,0)
 ;;=4^G20.
 ;;^UTILITY(U,$J,358.3,597,2)
 ;;=^5003770
 ;;^UTILITY(U,$J,358.3,598,0)
 ;;=G30.9^^6^73^1
 ;;^UTILITY(U,$J,358.3,598,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,598,1,3,0)
 ;;=3^Alzheimer's Disease,Unspec
 ;;^UTILITY(U,$J,358.3,598,1,4,0)
 ;;=4^G30.9
 ;;^UTILITY(U,$J,358.3,598,2)
 ;;=^5003808
 ;;^UTILITY(U,$J,358.3,599,0)
 ;;=G35.^^6^73^10
 ;;^UTILITY(U,$J,358.3,599,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,599,1,3,0)
 ;;=3^Multiple Sclerosis
 ;;^UTILITY(U,$J,358.3,599,1,4,0)
 ;;=4^G35.
 ;;^UTILITY(U,$J,358.3,599,2)
 ;;=^79761
 ;;^UTILITY(U,$J,358.3,600,0)
 ;;=G82.20^^6^73^11
 ;;^UTILITY(U,$J,358.3,600,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,600,1,3,0)
 ;;=3^Paraplegia,Unspec
 ;;^UTILITY(U,$J,358.3,600,1,4,0)
 ;;=4^G82.20
 ;;^UTILITY(U,$J,358.3,600,2)
 ;;=^5004125
 ;;^UTILITY(U,$J,358.3,601,0)
 ;;=R53.81^^6^73^9
 ;;^UTILITY(U,$J,358.3,601,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,601,1,3,0)
 ;;=3^Malaise
 ;;^UTILITY(U,$J,358.3,601,1,4,0)
 ;;=4^R53.81
 ;;^UTILITY(U,$J,358.3,601,2)
 ;;=^5019518
 ;;^UTILITY(U,$J,358.3,602,0)
 ;;=I69.91^^6^73^3
 ;;^UTILITY(U,$J,358.3,602,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,602,1,3,0)
 ;;=3^Cognitive Deficits Following CVD
 ;;^UTILITY(U,$J,358.3,602,1,4,0)
 ;;=4^I69.91
 ;;^UTILITY(U,$J,358.3,602,2)
 ;;=^5007552
 ;;^UTILITY(U,$J,358.3,603,0)
 ;;=I69.920^^6^73^2
 ;;^UTILITY(U,$J,358.3,603,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,603,1,3,0)
 ;;=3^Aphasia Following CVD
 ;;^UTILITY(U,$J,358.3,603,1,4,0)
 ;;=4^I69.920
 ;;^UTILITY(U,$J,358.3,603,2)
 ;;=^5007553
 ;;^UTILITY(U,$J,358.3,604,0)
 ;;=I69.921^^6^73^4
 ;;^UTILITY(U,$J,358.3,604,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,604,1,3,0)
 ;;=3^Dysphasia Following CVD
 ;;^UTILITY(U,$J,358.3,604,1,4,0)
 ;;=4^I69.921
 ;;^UTILITY(U,$J,358.3,604,2)
 ;;=^5007554
 ;;^UTILITY(U,$J,358.3,605,0)
 ;;=I69.928^^6^73^13
 ;;^UTILITY(U,$J,358.3,605,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,605,1,3,0)
 ;;=3^Speech/Lang Deficits Following CVD
 ;;^UTILITY(U,$J,358.3,605,1,4,0)
 ;;=4^I69.928
 ;;^UTILITY(U,$J,358.3,605,2)
 ;;=^5007557
 ;;^UTILITY(U,$J,358.3,606,0)
 ;;=I69.951^^6^73^7
 ;;^UTILITY(U,$J,358.3,606,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,606,1,3,0)
 ;;=3^Hemiplegia Following CVD,Right Dominant Side
 ;;^UTILITY(U,$J,358.3,606,1,4,0)
 ;;=4^I69.951
 ;;^UTILITY(U,$J,358.3,606,2)
 ;;=^5007561
 ;;^UTILITY(U,$J,358.3,607,0)
 ;;=I69.952^^6^73^5
 ;;^UTILITY(U,$J,358.3,607,1,0)
 ;;=^358.31IA^4^2
