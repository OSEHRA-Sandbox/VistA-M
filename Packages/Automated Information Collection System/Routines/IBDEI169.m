IBDEI169 ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,19930,1,3,0)
 ;;=3^Proteinuria Bence Jones
 ;;^UTILITY(U,$J,358.3,19930,1,4,0)
 ;;=4^R80.3
 ;;^UTILITY(U,$J,358.3,19930,2)
 ;;=^13272
 ;;^UTILITY(U,$J,358.3,19931,0)
 ;;=R80.9^^84^925^89
 ;;^UTILITY(U,$J,358.3,19931,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19931,1,3,0)
 ;;=3^Proteinuria,Unspec
 ;;^UTILITY(U,$J,358.3,19931,1,4,0)
 ;;=4^R80.9
 ;;^UTILITY(U,$J,358.3,19931,2)
 ;;=^5019599
 ;;^UTILITY(U,$J,358.3,19932,0)
 ;;=Z87.442^^84^925^77
 ;;^UTILITY(U,$J,358.3,19932,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19932,1,3,0)
 ;;=3^Personal Hx of Urinary Calculi
 ;;^UTILITY(U,$J,358.3,19932,1,4,0)
 ;;=4^Z87.442
 ;;^UTILITY(U,$J,358.3,19932,2)
 ;;=^5063497
 ;;^UTILITY(U,$J,358.3,19933,0)
 ;;=N18.1^^84^925^13
 ;;^UTILITY(U,$J,358.3,19933,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19933,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 1
 ;;^UTILITY(U,$J,358.3,19933,1,4,0)
 ;;=4^N18.1
 ;;^UTILITY(U,$J,358.3,19933,2)
 ;;=^5015602
 ;;^UTILITY(U,$J,358.3,19934,0)
 ;;=N18.2^^84^925^14
 ;;^UTILITY(U,$J,358.3,19934,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19934,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 2
 ;;^UTILITY(U,$J,358.3,19934,1,4,0)
 ;;=4^N18.2
 ;;^UTILITY(U,$J,358.3,19934,2)
 ;;=^5015603
 ;;^UTILITY(U,$J,358.3,19935,0)
 ;;=N18.3^^84^925^15
 ;;^UTILITY(U,$J,358.3,19935,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19935,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 3
 ;;^UTILITY(U,$J,358.3,19935,1,4,0)
 ;;=4^N18.3
 ;;^UTILITY(U,$J,358.3,19935,2)
 ;;=^5015604
 ;;^UTILITY(U,$J,358.3,19936,0)
 ;;=N18.4^^84^925^16
 ;;^UTILITY(U,$J,358.3,19936,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19936,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 4
 ;;^UTILITY(U,$J,358.3,19936,1,4,0)
 ;;=4^N18.4
 ;;^UTILITY(U,$J,358.3,19936,2)
 ;;=^5015605
 ;;^UTILITY(U,$J,358.3,19937,0)
 ;;=N18.5^^84^925^17
 ;;^UTILITY(U,$J,358.3,19937,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19937,1,3,0)
 ;;=3^Chronic Kidney Disease,Stage 5
 ;;^UTILITY(U,$J,358.3,19937,1,4,0)
 ;;=4^N18.5
 ;;^UTILITY(U,$J,358.3,19937,2)
 ;;=^5015606
 ;;^UTILITY(U,$J,358.3,19938,0)
 ;;=Q61.3^^84^925^80
 ;;^UTILITY(U,$J,358.3,19938,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19938,1,3,0)
 ;;=3^Polycystic Kidney,Unspec
 ;;^UTILITY(U,$J,358.3,19938,1,4,0)
 ;;=4^Q61.3
 ;;^UTILITY(U,$J,358.3,19938,2)
 ;;=^5018797
 ;;^UTILITY(U,$J,358.3,19939,0)
 ;;=N20.1^^84^925^9
 ;;^UTILITY(U,$J,358.3,19939,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19939,1,3,0)
 ;;=3^Calculus Ureter
 ;;^UTILITY(U,$J,358.3,19939,1,4,0)
 ;;=4^N20.1
 ;;^UTILITY(U,$J,358.3,19939,2)
 ;;=^5015608
 ;;^UTILITY(U,$J,358.3,19940,0)
 ;;=N18.9^^84^925^18
 ;;^UTILITY(U,$J,358.3,19940,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19940,1,3,0)
 ;;=3^Chronic Kidney Failure,Unspec
 ;;^UTILITY(U,$J,358.3,19940,1,4,0)
 ;;=4^N18.9
 ;;^UTILITY(U,$J,358.3,19940,2)
 ;;=^332812
 ;;^UTILITY(U,$J,358.3,19941,0)
 ;;=N18.6^^84^925^28
 ;;^UTILITY(U,$J,358.3,19941,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19941,1,3,0)
 ;;=3^End Stage Renal Disease (ESRD)
 ;;^UTILITY(U,$J,358.3,19941,1,4,0)
 ;;=4^N18.6
 ;;^UTILITY(U,$J,358.3,19941,2)
 ;;=^303986
 ;;^UTILITY(U,$J,358.3,19942,0)
 ;;=N52.9^^84^925^44
 ;;^UTILITY(U,$J,358.3,19942,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19942,1,3,0)
 ;;=3^Erectile Dysfunction,Unspec
 ;;^UTILITY(U,$J,358.3,19942,1,4,0)
 ;;=4^N52.9
 ;;^UTILITY(U,$J,358.3,19942,2)
 ;;=^5015763
 ;;^UTILITY(U,$J,358.3,19943,0)
 ;;=N32.81^^84^925^75
 ;;^UTILITY(U,$J,358.3,19943,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19943,1,3,0)
 ;;=3^Overactive Bladder
 ;;^UTILITY(U,$J,358.3,19943,1,4,0)
 ;;=4^N32.81
