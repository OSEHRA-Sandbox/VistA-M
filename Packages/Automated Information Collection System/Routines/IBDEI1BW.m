IBDEI1BW ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,23801,1,3,0)
 ;;=3^Foley Placement;Comp
 ;;^UTILITY(U,$J,358.3,23802,0)
 ;;=51700^^151^1516^3^^^^1
 ;;^UTILITY(U,$J,358.3,23802,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23802,1,2,0)
 ;;=2^51700
 ;;^UTILITY(U,$J,358.3,23802,1,3,0)
 ;;=3^Bladder Irrigation;Simple
 ;;^UTILITY(U,$J,358.3,23803,0)
 ;;=51720^^151^1516^2^^^^1
 ;;^UTILITY(U,$J,358.3,23803,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23803,1,2,0)
 ;;=2^51720
 ;;^UTILITY(U,$J,358.3,23803,1,3,0)
 ;;=3^Bladder Instill Anticarc
 ;;^UTILITY(U,$J,358.3,23804,0)
 ;;=90586^^151^1516^1^^^^1
 ;;^UTILITY(U,$J,358.3,23804,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23804,1,2,0)
 ;;=2^90586
 ;;^UTILITY(U,$J,358.3,23804,1,3,0)
 ;;=3^BCG for Bladder CA,Live
 ;;^UTILITY(U,$J,358.3,23805,0)
 ;;=51736^^151^1516^14^^^^1
 ;;^UTILITY(U,$J,358.3,23805,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23805,1,2,0)
 ;;=2^51736
 ;;^UTILITY(U,$J,358.3,23805,1,3,0)
 ;;=3^Uroflow;Simple
 ;;^UTILITY(U,$J,358.3,23806,0)
 ;;=51798^^151^1516^9^^^^1
 ;;^UTILITY(U,$J,358.3,23806,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23806,1,2,0)
 ;;=2^51798
 ;;^UTILITY(U,$J,358.3,23806,1,3,0)
 ;;=3^PVR by US
 ;;^UTILITY(U,$J,358.3,23807,0)
 ;;=54235^^151^1516^7^^^^1
 ;;^UTILITY(U,$J,358.3,23807,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23807,1,2,0)
 ;;=2^54235
 ;;^UTILITY(U,$J,358.3,23807,1,3,0)
 ;;=3^Inj of Corpora w/ Drugs
 ;;^UTILITY(U,$J,358.3,23808,0)
 ;;=99071^^151^1516^10^^^^1
 ;;^UTILITY(U,$J,358.3,23808,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23808,1,2,0)
 ;;=2^99071
 ;;^UTILITY(U,$J,358.3,23808,1,3,0)
 ;;=3^Patient Education Materials
 ;;^UTILITY(U,$J,358.3,23809,0)
 ;;=99078^^151^1516^6^^^^1
 ;;^UTILITY(U,$J,358.3,23809,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23809,1,2,0)
 ;;=2^99078
 ;;^UTILITY(U,$J,358.3,23809,1,3,0)
 ;;=3^Group Health Education
 ;;^UTILITY(U,$J,358.3,23810,0)
 ;;=98960^^151^1516^13^^^^1
 ;;^UTILITY(U,$J,358.3,23810,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23810,1,2,0)
 ;;=2^98960
 ;;^UTILITY(U,$J,358.3,23810,1,3,0)
 ;;=3^Patient Education,Indiv,30 Min
 ;;^UTILITY(U,$J,358.3,23811,0)
 ;;=98961^^151^1516^11^^^^1
 ;;^UTILITY(U,$J,358.3,23811,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23811,1,2,0)
 ;;=2^98961
 ;;^UTILITY(U,$J,358.3,23811,1,3,0)
 ;;=3^Patient Education,2-4 Pts,30 Min
 ;;^UTILITY(U,$J,358.3,23812,0)
 ;;=98962^^151^1516^12^^^^1
 ;;^UTILITY(U,$J,358.3,23812,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23812,1,2,0)
 ;;=2^98962
 ;;^UTILITY(U,$J,358.3,23812,1,3,0)
 ;;=3^Patient Education,5-8 Pts,30 Min
 ;;^UTILITY(U,$J,358.3,23813,0)
 ;;=99071^^151^1517^9^^^^1
 ;;^UTILITY(U,$J,358.3,23813,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23813,1,2,0)
 ;;=2^99071
 ;;^UTILITY(U,$J,358.3,23813,1,3,0)
 ;;=3^Patient Education Materials
 ;;^UTILITY(U,$J,358.3,23814,0)
 ;;=57160^^151^1517^6^^^^1
 ;;^UTILITY(U,$J,358.3,23814,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23814,1,2,0)
 ;;=2^57160
 ;;^UTILITY(U,$J,358.3,23814,1,3,0)
 ;;=3^Insert Pessary/Oth Device
 ;;^UTILITY(U,$J,358.3,23815,0)
 ;;=90656^^151^1517^5^^^^1
 ;;^UTILITY(U,$J,358.3,23815,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23815,1,2,0)
 ;;=2^90656
 ;;^UTILITY(U,$J,358.3,23815,1,3,0)
 ;;=3^Flu Vaccine No Preservatives
 ;;^UTILITY(U,$J,358.3,23816,0)
 ;;=90658^^151^1517^4^^^^1
 ;;^UTILITY(U,$J,358.3,23816,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23816,1,2,0)
 ;;=2^90658
 ;;^UTILITY(U,$J,358.3,23816,1,3,0)
 ;;=3^Flu Vaccine
 ;;^UTILITY(U,$J,358.3,23817,0)
 ;;=90715^^151^1517^12^^^^1
 ;;^UTILITY(U,$J,358.3,23817,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,23817,1,2,0)
 ;;=2^90715
