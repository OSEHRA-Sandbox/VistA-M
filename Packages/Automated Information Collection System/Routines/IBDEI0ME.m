IBDEI0ME ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,10994,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10994,1,3,0)
 ;;=3^V87.39
 ;;^UTILITY(U,$J,358.3,10994,1,4,0)
 ;;=4^CONT/EXP HAZARD SUB NEC
 ;;^UTILITY(U,$J,358.3,10994,2)
 ;;=^336815
 ;;^UTILITY(U,$J,358.3,10995,0)
 ;;=530.0^^71^721^1
 ;;^UTILITY(U,$J,358.3,10995,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,10995,1,2,0)
 ;;=2^530.0
 ;;^UTILITY(U,$J,358.3,10995,1,5,0)
 ;;=5^Achalasia 
 ;;^UTILITY(U,$J,358.3,10995,2)
 ;;=^42424
 ;;^UTILITY(U,$J,358.3,10996,0)
 ;;=150.9^^71^721^13
 ;;^UTILITY(U,$J,358.3,10996,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,10996,1,2,0)
 ;;=2^150.9
 ;;^UTILITY(U,$J,358.3,10996,1,5,0)
 ;;=5^Malig Neo Esophagus NOS
 ;;^UTILITY(U,$J,358.3,10996,2)
 ;;=^267055
 ;;^UTILITY(U,$J,358.3,10997,0)
 ;;=530.10^^71^721^9
 ;;^UTILITY(U,$J,358.3,10997,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,10997,1,2,0)
 ;;=2^530.10
 ;;^UTILITY(U,$J,358.3,10997,1,5,0)
 ;;=5^Esophagitis, Unsp.
 ;;^UTILITY(U,$J,358.3,10997,2)
 ;;=^295809
 ;;^UTILITY(U,$J,358.3,10998,0)
 ;;=112.84^^71^721^7
 ;;^UTILITY(U,$J,358.3,10998,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,10998,1,2,0)
 ;;=2^112.84
 ;;^UTILITY(U,$J,358.3,10998,1,5,0)
 ;;=5^Esophagitis Candida
 ;;^UTILITY(U,$J,358.3,10998,2)
 ;;=^259729
 ;;^UTILITY(U,$J,358.3,10999,0)
 ;;=530.11^^71^721^8
 ;;^UTILITY(U,$J,358.3,10999,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,10999,1,2,0)
 ;;=2^530.11
 ;;^UTILITY(U,$J,358.3,10999,1,5,0)
 ;;=5^Esophagitis d/t Reflux
 ;;^UTILITY(U,$J,358.3,10999,2)
 ;;=Esophagitis due to Reflux^295747
 ;;^UTILITY(U,$J,358.3,11000,0)
 ;;=530.81^^71^721^5
 ;;^UTILITY(U,$J,358.3,11000,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11000,1,2,0)
 ;;=2^530.81
 ;;^UTILITY(U,$J,358.3,11000,1,5,0)
 ;;=5^Esophageal Reflux
 ;;^UTILITY(U,$J,358.3,11000,2)
 ;;=^295749
 ;;^UTILITY(U,$J,358.3,11001,0)
 ;;=787.1^^71^721^11
 ;;^UTILITY(U,$J,358.3,11001,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11001,1,2,0)
 ;;=2^787.1
 ;;^UTILITY(U,$J,358.3,11001,1,5,0)
 ;;=5^Heartburn
 ;;^UTILITY(U,$J,358.3,11001,2)
 ;;=^54996
 ;;^UTILITY(U,$J,358.3,11002,0)
 ;;=553.3^^71^721^12
 ;;^UTILITY(U,$J,358.3,11002,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11002,1,2,0)
 ;;=2^553.3
 ;;^UTILITY(U,$J,358.3,11002,1,5,0)
 ;;=5^Hiatal Hernia
 ;;^UTILITY(U,$J,358.3,11002,2)
 ;;=^33903
 ;;^UTILITY(U,$J,358.3,11003,0)
 ;;=530.7^^71^721^17
 ;;^UTILITY(U,$J,358.3,11003,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11003,1,2,0)
 ;;=2^530.7
 ;;^UTILITY(U,$J,358.3,11003,1,5,0)
 ;;=5^Mallory-Weiss Syndrome
 ;;^UTILITY(U,$J,358.3,11003,2)
 ;;=^49479
 ;;^UTILITY(U,$J,358.3,11004,0)
 ;;=530.3^^71^721^6
 ;;^UTILITY(U,$J,358.3,11004,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11004,1,2,0)
 ;;=2^530.3
 ;;^UTILITY(U,$J,358.3,11004,1,5,0)
 ;;=5^Esophageal Stricture
 ;;^UTILITY(U,$J,358.3,11004,2)
 ;;=Esophageal Stricture^114760
 ;;^UTILITY(U,$J,358.3,11005,0)
 ;;=456.0^^71^721^19
 ;;^UTILITY(U,$J,358.3,11005,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11005,1,2,0)
 ;;=2^456.0
 ;;^UTILITY(U,$J,358.3,11005,1,5,0)
 ;;=5^Varices w/ Bleed
 ;;^UTILITY(U,$J,358.3,11005,2)
 ;;=Varices with bleed^269835
 ;;^UTILITY(U,$J,358.3,11006,0)
 ;;=456.1^^71^721^20
 ;;^UTILITY(U,$J,358.3,11006,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11006,1,2,0)
 ;;=2^456.1
 ;;^UTILITY(U,$J,358.3,11006,1,5,0)
 ;;=5^Varices w/o Bleed
 ;;^UTILITY(U,$J,358.3,11006,2)
 ;;=^269836
 ;;^UTILITY(U,$J,358.3,11007,0)
 ;;=530.6^^71^721^21
 ;;^UTILITY(U,$J,358.3,11007,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,11007,1,2,0)
 ;;=2^530.6
 ;;^UTILITY(U,$J,358.3,11007,1,5,0)
 ;;=5^Zenker's Diverticulum
