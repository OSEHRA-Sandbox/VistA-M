IBDEI29W ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,38171,1,3,0)
 ;;=3^Lack of Adequate Food or Safe Drinking Water
 ;;^UTILITY(U,$J,358.3,38171,1,4,0)
 ;;=4^Z59.4
 ;;^UTILITY(U,$J,358.3,38171,2)
 ;;=^5063133
 ;;^UTILITY(U,$J,358.3,38172,0)
 ;;=Z59.5^^177^1924^2
 ;;^UTILITY(U,$J,358.3,38172,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38172,1,3,0)
 ;;=3^Extreme Poverty
 ;;^UTILITY(U,$J,358.3,38172,1,4,0)
 ;;=4^Z59.5
 ;;^UTILITY(U,$J,358.3,38172,2)
 ;;=^5063134
 ;;^UTILITY(U,$J,358.3,38173,0)
 ;;=Z59.6^^177^1924^8
 ;;^UTILITY(U,$J,358.3,38173,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38173,1,3,0)
 ;;=3^Low Income
 ;;^UTILITY(U,$J,358.3,38173,1,4,0)
 ;;=4^Z59.6
 ;;^UTILITY(U,$J,358.3,38173,2)
 ;;=^5063135
 ;;^UTILITY(U,$J,358.3,38174,0)
 ;;=Z59.7^^177^1924^6
 ;;^UTILITY(U,$J,358.3,38174,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38174,1,3,0)
 ;;=3^Insufficient Social Insurance/Welfare Support
 ;;^UTILITY(U,$J,358.3,38174,1,4,0)
 ;;=4^Z59.7
 ;;^UTILITY(U,$J,358.3,38174,2)
 ;;=^5063136
 ;;^UTILITY(U,$J,358.3,38175,0)
 ;;=Z59.9^^177^1924^4
 ;;^UTILITY(U,$J,358.3,38175,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38175,1,3,0)
 ;;=3^Housing/Economic Problems,Unspec
 ;;^UTILITY(U,$J,358.3,38175,1,4,0)
 ;;=4^Z59.9
 ;;^UTILITY(U,$J,358.3,38175,2)
 ;;=^5063138
 ;;^UTILITY(U,$J,358.3,38176,0)
 ;;=G21.19^^177^1925^3
 ;;^UTILITY(U,$J,358.3,38176,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38176,1,3,0)
 ;;=3^Medication-Induced Parkinsonism NEC
 ;;^UTILITY(U,$J,358.3,38176,1,4,0)
 ;;=4^G21.19
 ;;^UTILITY(U,$J,358.3,38176,2)
 ;;=^5003773
 ;;^UTILITY(U,$J,358.3,38177,0)
 ;;=G21.11^^177^1925^5
 ;;^UTILITY(U,$J,358.3,38177,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38177,1,3,0)
 ;;=3^Neuroleptic-Induced Parkinsonism
 ;;^UTILITY(U,$J,358.3,38177,1,4,0)
 ;;=4^G21.11
 ;;^UTILITY(U,$J,358.3,38177,2)
 ;;=^5003772
 ;;^UTILITY(U,$J,358.3,38178,0)
 ;;=G24.02^^177^1925^1
 ;;^UTILITY(U,$J,358.3,38178,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38178,1,3,0)
 ;;=3^Medication-Induced Acute Dystonia
 ;;^UTILITY(U,$J,358.3,38178,1,4,0)
 ;;=4^G24.02
 ;;^UTILITY(U,$J,358.3,38178,2)
 ;;=^5003785
 ;;^UTILITY(U,$J,358.3,38179,0)
 ;;=G24.01^^177^1925^7
 ;;^UTILITY(U,$J,358.3,38179,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38179,1,3,0)
 ;;=3^Tardive Dyskinesia
 ;;^UTILITY(U,$J,358.3,38179,1,4,0)
 ;;=4^G24.01
 ;;^UTILITY(U,$J,358.3,38179,2)
 ;;=^5003784
 ;;^UTILITY(U,$J,358.3,38180,0)
 ;;=G24.09^^177^1925^8
 ;;^UTILITY(U,$J,358.3,38180,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38180,1,3,0)
 ;;=3^Tardive Dystonia
 ;;^UTILITY(U,$J,358.3,38180,1,4,0)
 ;;=4^G24.09
 ;;^UTILITY(U,$J,358.3,38180,2)
 ;;=^5003786
 ;;^UTILITY(U,$J,358.3,38181,0)
 ;;=G25.1^^177^1925^4
 ;;^UTILITY(U,$J,358.3,38181,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38181,1,3,0)
 ;;=3^Medication-Induced Postural Tremor
 ;;^UTILITY(U,$J,358.3,38181,1,4,0)
 ;;=4^G25.1
 ;;^UTILITY(U,$J,358.3,38181,2)
 ;;=^5003792
 ;;^UTILITY(U,$J,358.3,38182,0)
 ;;=G25.71^^177^1925^6
 ;;^UTILITY(U,$J,358.3,38182,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38182,1,3,0)
 ;;=3^Tardive Akathisia/Medication-Induced Acute Akatisia
 ;;^UTILITY(U,$J,358.3,38182,1,4,0)
 ;;=4^G25.71
 ;;^UTILITY(U,$J,358.3,38182,2)
 ;;=^5003799
 ;;^UTILITY(U,$J,358.3,38183,0)
 ;;=G25.79^^177^1925^2
 ;;^UTILITY(U,$J,358.3,38183,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38183,1,3,0)
 ;;=3^Medication-Induced Movement Disorder NEC
 ;;^UTILITY(U,$J,358.3,38183,1,4,0)
 ;;=4^G25.79
 ;;^UTILITY(U,$J,358.3,38183,2)
 ;;=^5003800
 ;;^UTILITY(U,$J,358.3,38184,0)
 ;;=F42.^^177^1926^6
 ;;^UTILITY(U,$J,358.3,38184,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38184,1,3,0)
 ;;=3^Obsessive-Compulsive Disorder
