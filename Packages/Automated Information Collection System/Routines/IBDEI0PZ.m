IBDEI0PZ ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,11899,1,4,0)
 ;;=4^V00.832D
 ;;^UTILITY(U,$J,358.3,11899,2)
 ;;=^5055959
 ;;^UTILITY(U,$J,358.3,11900,0)
 ;;=V00.838A^^68^694^93
 ;;^UTILITY(U,$J,358.3,11900,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11900,1,3,0)
 ;;=3^Mobility Scooter Accident NEC,Init Encntr
 ;;^UTILITY(U,$J,358.3,11900,1,4,0)
 ;;=4^V00.838A
 ;;^UTILITY(U,$J,358.3,11900,2)
 ;;=^5055961
 ;;^UTILITY(U,$J,358.3,11901,0)
 ;;=V00.838D^^68^694^94
 ;;^UTILITY(U,$J,358.3,11901,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11901,1,3,0)
 ;;=3^Mobility Scooter Accident NEC,Subs Encntr
 ;;^UTILITY(U,$J,358.3,11901,1,4,0)
 ;;=4^V00.838D
 ;;^UTILITY(U,$J,358.3,11901,2)
 ;;=^5055962
 ;;^UTILITY(U,$J,358.3,11902,0)
 ;;=V00.891A^^68^694^57
 ;;^UTILITY(U,$J,358.3,11902,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11902,1,3,0)
 ;;=3^Fall from Pedestrian Conveyance,Init Encntr
 ;;^UTILITY(U,$J,358.3,11902,1,4,0)
 ;;=4^V00.891A
 ;;^UTILITY(U,$J,358.3,11902,2)
 ;;=^5055964
 ;;^UTILITY(U,$J,358.3,11903,0)
 ;;=V00.891D^^68^694^58
 ;;^UTILITY(U,$J,358.3,11903,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11903,1,3,0)
 ;;=3^Fall from Pedestrian Conveyance,Subs Encntr
 ;;^UTILITY(U,$J,358.3,11903,1,4,0)
 ;;=4^V00.891D
 ;;^UTILITY(U,$J,358.3,11903,2)
 ;;=^5055965
 ;;^UTILITY(U,$J,358.3,11904,0)
 ;;=V00.892A^^68^694^99
 ;;^UTILITY(U,$J,358.3,11904,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11904,1,3,0)
 ;;=3^Pedestrian Conveyance Colliding w/ Stationary Obj,Init Encntr
 ;;^UTILITY(U,$J,358.3,11904,1,4,0)
 ;;=4^V00.892A
 ;;^UTILITY(U,$J,358.3,11904,2)
 ;;=^5055967
 ;;^UTILITY(U,$J,358.3,11905,0)
 ;;=V00.892D^^68^694^100
 ;;^UTILITY(U,$J,358.3,11905,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11905,1,3,0)
 ;;=3^Pedestrian Conveyance Colliding w/ Stationary Obj,Subs Encntr
 ;;^UTILITY(U,$J,358.3,11905,1,4,0)
 ;;=4^V00.892D
 ;;^UTILITY(U,$J,358.3,11905,2)
 ;;=^5055968
 ;;^UTILITY(U,$J,358.3,11906,0)
 ;;=V00.898A^^68^694^97
 ;;^UTILITY(U,$J,358.3,11906,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11906,1,3,0)
 ;;=3^Pedestrian Conveyance Accident NEC,Init Encntr
 ;;^UTILITY(U,$J,358.3,11906,1,4,0)
 ;;=4^V00.898A
 ;;^UTILITY(U,$J,358.3,11906,2)
 ;;=^5055970
 ;;^UTILITY(U,$J,358.3,11907,0)
 ;;=V00.898D^^68^694^98
 ;;^UTILITY(U,$J,358.3,11907,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11907,1,3,0)
 ;;=3^Pedestrian Conveyance Accident NEC,Subs Encntr
 ;;^UTILITY(U,$J,358.3,11907,1,4,0)
 ;;=4^V00.898D
 ;;^UTILITY(U,$J,358.3,11907,2)
 ;;=^5055971
 ;;^UTILITY(U,$J,358.3,11908,0)
 ;;=W00.0XXA^^68^694^79
 ;;^UTILITY(U,$J,358.3,11908,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11908,1,3,0)
 ;;=3^Fall on Same Level d/t Ice/Snow,Init Encntr
 ;;^UTILITY(U,$J,358.3,11908,1,4,0)
 ;;=4^W00.0XXA
 ;;^UTILITY(U,$J,358.3,11908,2)
 ;;=^5059510
 ;;^UTILITY(U,$J,358.3,11909,0)
 ;;=W00.0XXD^^68^694^80
 ;;^UTILITY(U,$J,358.3,11909,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11909,1,3,0)
 ;;=3^Fall on Same Level d/t Ice/Snow,Subs Encntr
 ;;^UTILITY(U,$J,358.3,11909,1,4,0)
 ;;=4^W00.0XXD
 ;;^UTILITY(U,$J,358.3,11909,2)
 ;;=^5059511
 ;;^UTILITY(U,$J,358.3,11910,0)
 ;;=W00.1XXA^^68^694^59
 ;;^UTILITY(U,$J,358.3,11910,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11910,1,3,0)
 ;;=3^Fall from Stairs/Steps d/t Ice/Snow,Init Encntr
 ;;^UTILITY(U,$J,358.3,11910,1,4,0)
 ;;=4^W00.1XXA
 ;;^UTILITY(U,$J,358.3,11910,2)
 ;;=^5059513
 ;;^UTILITY(U,$J,358.3,11911,0)
 ;;=W00.1XXD^^68^694^60
 ;;^UTILITY(U,$J,358.3,11911,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11911,1,3,0)
 ;;=3^Fall from Stairs/Steps d/t Ice/Snow,Subs Encntr
 ;;^UTILITY(U,$J,358.3,11911,1,4,0)
 ;;=4^W00.1XXD
