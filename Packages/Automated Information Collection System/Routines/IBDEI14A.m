IBDEI14A ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,19000,0)
 ;;=H57.00^^82^903^2
 ;;^UTILITY(U,$J,358.3,19000,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19000,1,3,0)
 ;;=3^Anomaly of Pupillary Function,Unspec
 ;;^UTILITY(U,$J,358.3,19000,1,4,0)
 ;;=4^H57.00
 ;;^UTILITY(U,$J,358.3,19000,2)
 ;;=^5006375
 ;;^UTILITY(U,$J,358.3,19001,0)
 ;;=H57.9^^82^903^9
 ;;^UTILITY(U,$J,358.3,19001,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19001,1,3,0)
 ;;=3^Eye/Adnexa Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,19001,1,4,0)
 ;;=4^H57.9
 ;;^UTILITY(U,$J,358.3,19001,2)
 ;;=^269333
 ;;^UTILITY(U,$J,358.3,19002,0)
 ;;=H57.02^^82^903^1
 ;;^UTILITY(U,$J,358.3,19002,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19002,1,3,0)
 ;;=3^Anisocoria
 ;;^UTILITY(U,$J,358.3,19002,1,4,0)
 ;;=4^H57.02
 ;;^UTILITY(U,$J,358.3,19002,2)
 ;;=^7834
 ;;^UTILITY(U,$J,358.3,19003,0)
 ;;=B39.9^^82^904^20
 ;;^UTILITY(U,$J,358.3,19003,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19003,1,3,0)
 ;;=3^Histoplasmosis,Unspec
 ;;^UTILITY(U,$J,358.3,19003,1,4,0)
 ;;=4^B39.9
 ;;^UTILITY(U,$J,358.3,19003,2)
 ;;=^5000638
 ;;^UTILITY(U,$J,358.3,19004,0)
 ;;=H32.^^82^904^10
 ;;^UTILITY(U,$J,358.3,19004,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19004,1,3,0)
 ;;=3^Chorioretinal Disorders in Diseases Classified Elsewhere
 ;;^UTILITY(U,$J,358.3,19004,1,4,0)
 ;;=4^H32.
 ;;^UTILITY(U,$J,358.3,19004,2)
 ;;=^5005489
 ;;^UTILITY(U,$J,358.3,19005,0)
 ;;=H33.101^^82^904^47
 ;;^UTILITY(U,$J,358.3,19005,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19005,1,3,0)
 ;;=3^Retinoschisis,Right Eye,Unspec
 ;;^UTILITY(U,$J,358.3,19005,1,4,0)
 ;;=4^H33.101
 ;;^UTILITY(U,$J,358.3,19005,2)
 ;;=^5005513
 ;;^UTILITY(U,$J,358.3,19006,0)
 ;;=H33.102^^82^904^46
 ;;^UTILITY(U,$J,358.3,19006,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19006,1,3,0)
 ;;=3^Retinoschisis,Left Eye,Unspec
 ;;^UTILITY(U,$J,358.3,19006,1,4,0)
 ;;=4^H33.102
 ;;^UTILITY(U,$J,358.3,19006,2)
 ;;=^5005514
 ;;^UTILITY(U,$J,358.3,19007,0)
 ;;=H33.21^^82^904^50
 ;;^UTILITY(U,$J,358.3,19007,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19007,1,3,0)
 ;;=3^Serous Retinal Detachment,Right Eye
 ;;^UTILITY(U,$J,358.3,19007,1,4,0)
 ;;=4^H33.21
 ;;^UTILITY(U,$J,358.3,19007,2)
 ;;=^5005529
 ;;^UTILITY(U,$J,358.3,19008,0)
 ;;=H33.22^^82^904^49
 ;;^UTILITY(U,$J,358.3,19008,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19008,1,3,0)
 ;;=3^Serous Retinal Detachment,Left Eye
 ;;^UTILITY(U,$J,358.3,19008,1,4,0)
 ;;=4^H33.22
 ;;^UTILITY(U,$J,358.3,19008,2)
 ;;=^5005530
 ;;^UTILITY(U,$J,358.3,19009,0)
 ;;=H33.23^^82^904^48
 ;;^UTILITY(U,$J,358.3,19009,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19009,1,3,0)
 ;;=3^Serous Retinal Detachment,Bilateral
 ;;^UTILITY(U,$J,358.3,19009,1,4,0)
 ;;=4^H33.23
 ;;^UTILITY(U,$J,358.3,19009,2)
 ;;=^5005531
 ;;^UTILITY(U,$J,358.3,19010,0)
 ;;=H33.001^^82^904^40
 ;;^UTILITY(U,$J,358.3,19010,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19010,1,3,0)
 ;;=3^Retinal Detachment w/ Retinal Break,Right Eye
 ;;^UTILITY(U,$J,358.3,19010,1,4,0)
 ;;=4^H33.001
 ;;^UTILITY(U,$J,358.3,19010,2)
 ;;=^5005490
 ;;^UTILITY(U,$J,358.3,19011,0)
 ;;=H33.002^^82^904^39
 ;;^UTILITY(U,$J,358.3,19011,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19011,1,3,0)
 ;;=3^Retinal Detachment w/ Retinal Break,Left Eye
 ;;^UTILITY(U,$J,358.3,19011,1,4,0)
 ;;=4^H33.002
 ;;^UTILITY(U,$J,358.3,19011,2)
 ;;=^5005491
 ;;^UTILITY(U,$J,358.3,19012,0)
 ;;=H33.003^^82^904^38
 ;;^UTILITY(U,$J,358.3,19012,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19012,1,3,0)
 ;;=3^Retinal Detachment w/ Retinal Break,Bilateral
 ;;^UTILITY(U,$J,358.3,19012,1,4,0)
 ;;=4^H33.003
 ;;^UTILITY(U,$J,358.3,19012,2)
 ;;=^5005492
