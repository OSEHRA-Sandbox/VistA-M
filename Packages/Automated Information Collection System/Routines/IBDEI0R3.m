IBDEI0R3 ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,12709,0)
 ;;=B35.3^^53^582^283
 ;;^UTILITY(U,$J,358.3,12709,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12709,1,3,0)
 ;;=3^Tinea Pedis
 ;;^UTILITY(U,$J,358.3,12709,1,4,0)
 ;;=4^B35.3
 ;;^UTILITY(U,$J,358.3,12709,2)
 ;;=^119732
 ;;^UTILITY(U,$J,358.3,12710,0)
 ;;=B35.5^^53^582^282
 ;;^UTILITY(U,$J,358.3,12710,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12710,1,3,0)
 ;;=3^Tinea Imbricata
 ;;^UTILITY(U,$J,358.3,12710,1,4,0)
 ;;=4^B35.5
 ;;^UTILITY(U,$J,358.3,12710,2)
 ;;=^119725
 ;;^UTILITY(U,$J,358.3,12711,0)
 ;;=B35.4^^53^582^280
 ;;^UTILITY(U,$J,358.3,12711,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12711,1,3,0)
 ;;=3^Tinea Corporis
 ;;^UTILITY(U,$J,358.3,12711,1,4,0)
 ;;=4^B35.4
 ;;^UTILITY(U,$J,358.3,12711,2)
 ;;=^119704
 ;;^UTILITY(U,$J,358.3,12712,0)
 ;;=B35.8^^53^582^137
 ;;^UTILITY(U,$J,358.3,12712,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12712,1,3,0)
 ;;=3^Dermatophytoses,Other
 ;;^UTILITY(U,$J,358.3,12712,1,4,0)
 ;;=4^B35.8
 ;;^UTILITY(U,$J,358.3,12712,2)
 ;;=^5000606
 ;;^UTILITY(U,$J,358.3,12713,0)
 ;;=B36.9^^53^582^272
 ;;^UTILITY(U,$J,358.3,12713,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12713,1,3,0)
 ;;=3^Superficial Mycosis,Unspec
 ;;^UTILITY(U,$J,358.3,12713,1,4,0)
 ;;=4^B36.9
 ;;^UTILITY(U,$J,358.3,12713,2)
 ;;=^5000611
 ;;^UTILITY(U,$J,358.3,12714,0)
 ;;=D69.0^^53^582^32
 ;;^UTILITY(U,$J,358.3,12714,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12714,1,3,0)
 ;;=3^Allergic Purpura
 ;;^UTILITY(U,$J,358.3,12714,1,4,0)
 ;;=4^D69.0
 ;;^UTILITY(U,$J,358.3,12714,2)
 ;;=^5002365
 ;;^UTILITY(U,$J,358.3,12715,0)
 ;;=B00.9^^53^582^158
 ;;^UTILITY(U,$J,358.3,12715,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12715,1,3,0)
 ;;=3^Herpesviral Infection,Unspec
 ;;^UTILITY(U,$J,358.3,12715,1,4,0)
 ;;=4^B00.9
 ;;^UTILITY(U,$J,358.3,12715,2)
 ;;=^5000480
 ;;^UTILITY(U,$J,358.3,12716,0)
 ;;=B02.9^^53^582^291
 ;;^UTILITY(U,$J,358.3,12716,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12716,1,3,0)
 ;;=3^Zoster w/o Complications
 ;;^UTILITY(U,$J,358.3,12716,1,4,0)
 ;;=4^B02.9
 ;;^UTILITY(U,$J,358.3,12716,2)
 ;;=^5000501
 ;;^UTILITY(U,$J,358.3,12717,0)
 ;;=D17.9^^53^582^89
 ;;^UTILITY(U,$J,358.3,12717,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12717,1,3,0)
 ;;=3^Benign Lipomatous Neop,Unspec
 ;;^UTILITY(U,$J,358.3,12717,1,4,0)
 ;;=4^D17.9
 ;;^UTILITY(U,$J,358.3,12717,2)
 ;;=^5002020
 ;;^UTILITY(U,$J,358.3,12718,0)
 ;;=E08.621^^53^582^134
 ;;^UTILITY(U,$J,358.3,12718,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12718,1,3,0)
 ;;=3^DM d/t Underlying Condition w/ Foot Ulcer
 ;;^UTILITY(U,$J,358.3,12718,1,4,0)
 ;;=4^E08.621
 ;;^UTILITY(U,$J,358.3,12718,2)
 ;;=^5002534
 ;;^UTILITY(U,$J,358.3,12719,0)
 ;;=E09.621^^53^582^133
 ;;^UTILITY(U,$J,358.3,12719,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12719,1,3,0)
 ;;=3^DM d/t Drug/Chemical w/ Foot Ulcer
 ;;^UTILITY(U,$J,358.3,12719,1,4,0)
 ;;=4^E09.621
 ;;^UTILITY(U,$J,358.3,12719,2)
 ;;=^5002576
 ;;^UTILITY(U,$J,358.3,12720,0)
 ;;=H05.011^^53^582^110
 ;;^UTILITY(U,$J,358.3,12720,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12720,1,3,0)
 ;;=3^Cellulitis of Right Orbit
 ;;^UTILITY(U,$J,358.3,12720,1,4,0)
 ;;=4^H05.011
 ;;^UTILITY(U,$J,358.3,12720,2)
 ;;=^5004560
 ;;^UTILITY(U,$J,358.3,12721,0)
 ;;=H05.012^^53^582^103
 ;;^UTILITY(U,$J,358.3,12721,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12721,1,3,0)
 ;;=3^Cellulitis of Left Orbit
 ;;^UTILITY(U,$J,358.3,12721,1,4,0)
 ;;=4^H05.012
 ;;^UTILITY(U,$J,358.3,12721,2)
 ;;=^5004561
 ;;^UTILITY(U,$J,358.3,12722,0)
 ;;=H05.013^^53^582^97
 ;;^UTILITY(U,$J,358.3,12722,1,0)
 ;;=^358.31IA^4^2
