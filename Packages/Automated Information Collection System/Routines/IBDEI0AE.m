IBDEI0AE ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,4840,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4840,1,2,0)
 ;;=2^702.19
 ;;^UTILITY(U,$J,358.3,4840,1,5,0)
 ;;=5^Seborrheic Keratosis
 ;;^UTILITY(U,$J,358.3,4840,2)
 ;;=^303312
 ;;^UTILITY(U,$J,358.3,4841,0)
 ;;=702.11^^39^376^2
 ;;^UTILITY(U,$J,358.3,4841,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4841,1,2,0)
 ;;=2^702.11
 ;;^UTILITY(U,$J,358.3,4841,1,5,0)
 ;;=5^Inflamed Seborrheic Keratosis
 ;;^UTILITY(U,$J,358.3,4841,2)
 ;;=^303311
 ;;^UTILITY(U,$J,358.3,4842,0)
 ;;=702.8^^39^376^3
 ;;^UTILITY(U,$J,358.3,4842,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4842,1,2,0)
 ;;=2^702.8
 ;;^UTILITY(U,$J,358.3,4842,1,5,0)
 ;;=5^Other Specified Dermatoses
 ;;^UTILITY(U,$J,358.3,4842,2)
 ;;=^271925
 ;;^UTILITY(U,$J,358.3,4843,0)
 ;;=880.03^^39^377^1
 ;;^UTILITY(U,$J,358.3,4843,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4843,1,2,0)
 ;;=2^880.03
 ;;^UTILITY(U,$J,358.3,4843,1,5,0)
 ;;=5^Laceration Arm, Upper
 ;;^UTILITY(U,$J,358.3,4843,2)
 ;;=^275028
 ;;^UTILITY(U,$J,358.3,4844,0)
 ;;=873.42^^39^377^2
 ;;^UTILITY(U,$J,358.3,4844,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4844,1,2,0)
 ;;=2^873.42
 ;;^UTILITY(U,$J,358.3,4844,1,5,0)
 ;;=5^Laceration Brow
 ;;^UTILITY(U,$J,358.3,4844,2)
 ;;=^274943
 ;;^UTILITY(U,$J,358.3,4845,0)
 ;;=873.41^^39^377^3
 ;;^UTILITY(U,$J,358.3,4845,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4845,1,2,0)
 ;;=2^873.41
 ;;^UTILITY(U,$J,358.3,4845,1,5,0)
 ;;=5^Laceration Cheek
 ;;^UTILITY(U,$J,358.3,4845,2)
 ;;=^274940
 ;;^UTILITY(U,$J,358.3,4846,0)
 ;;=872.8^^39^377^4
 ;;^UTILITY(U,$J,358.3,4846,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4846,1,2,0)
 ;;=2^872.8
 ;;^UTILITY(U,$J,358.3,4846,1,5,0)
 ;;=5^Laceration Ear
 ;;^UTILITY(U,$J,358.3,4846,2)
 ;;=^274918
 ;;^UTILITY(U,$J,358.3,4847,0)
 ;;=873.40^^39^377^5
 ;;^UTILITY(U,$J,358.3,4847,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4847,1,2,0)
 ;;=2^873.40
 ;;^UTILITY(U,$J,358.3,4847,1,5,0)
 ;;=5^Laceration Face
 ;;^UTILITY(U,$J,358.3,4847,2)
 ;;=^274939
 ;;^UTILITY(U,$J,358.3,4848,0)
 ;;=881.00^^39^377^6
 ;;^UTILITY(U,$J,358.3,4848,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4848,1,2,0)
 ;;=2^881.00
 ;;^UTILITY(U,$J,358.3,4848,1,5,0)
 ;;=5^Laceration Forearm
 ;;^UTILITY(U,$J,358.3,4848,2)
 ;;=^275044
 ;;^UTILITY(U,$J,358.3,4849,0)
 ;;=891.0^^39^377^7
 ;;^UTILITY(U,$J,358.3,4849,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4849,1,2,0)
 ;;=2^891.0
 ;;^UTILITY(U,$J,358.3,4849,1,5,0)
 ;;=5^Laceration Leg, Lower
 ;;^UTILITY(U,$J,358.3,4849,2)
 ;;=^275087
 ;;^UTILITY(U,$J,358.3,4850,0)
 ;;=890.0^^39^377^8
 ;;^UTILITY(U,$J,358.3,4850,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4850,1,2,0)
 ;;=2^890.0
 ;;^UTILITY(U,$J,358.3,4850,1,5,0)
 ;;=5^Laceration Leg, Upper
 ;;^UTILITY(U,$J,358.3,4850,2)
 ;;=^275083
 ;;^UTILITY(U,$J,358.3,4851,0)
 ;;=873.20^^39^377^9
 ;;^UTILITY(U,$J,358.3,4851,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4851,1,2,0)
 ;;=2^873.20
 ;;^UTILITY(U,$J,358.3,4851,1,5,0)
 ;;=5^Laceration Nose
 ;;^UTILITY(U,$J,358.3,4851,2)
 ;;=^274924
 ;;^UTILITY(U,$J,358.3,4852,0)
 ;;=873.0^^39^377^10
 ;;^UTILITY(U,$J,358.3,4852,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4852,1,2,0)
 ;;=2^873.0
 ;;^UTILITY(U,$J,358.3,4852,1,5,0)
 ;;=5^Laceration Scalp
 ;;^UTILITY(U,$J,358.3,4852,2)
 ;;=^274921
 ;;^UTILITY(U,$J,358.3,4853,0)
 ;;=879.8^^39^377^11
 ;;^UTILITY(U,$J,358.3,4853,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4853,1,2,0)
 ;;=2^879.8
 ;;^UTILITY(U,$J,358.3,4853,1,5,0)
 ;;=5^Laceration NEC
 ;;^UTILITY(U,$J,358.3,4853,2)
 ;;=^275021
 ;;^UTILITY(U,$J,358.3,4854,0)
 ;;=709.9^^39^378^8
 ;;^UTILITY(U,$J,358.3,4854,1,0)
 ;;=^358.31IA^5^2
