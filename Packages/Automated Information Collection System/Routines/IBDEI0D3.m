IBDEI0D3 ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,6006,0)
 ;;=L89.301^^30^385^237
 ;;^UTILITY(U,$J,358.3,6006,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6006,1,3,0)
 ;;=3^Pressure Ulcer of Buttock,Stage 1
 ;;^UTILITY(U,$J,358.3,6006,1,4,0)
 ;;=4^L89.301
 ;;^UTILITY(U,$J,358.3,6006,2)
 ;;=^5009390
 ;;^UTILITY(U,$J,358.3,6007,0)
 ;;=L89.302^^30^385^238
 ;;^UTILITY(U,$J,358.3,6007,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6007,1,3,0)
 ;;=3^Pressure Ulcer of Buttock,Stage 2
 ;;^UTILITY(U,$J,358.3,6007,1,4,0)
 ;;=4^L89.302
 ;;^UTILITY(U,$J,358.3,6007,2)
 ;;=^5009391
 ;;^UTILITY(U,$J,358.3,6008,0)
 ;;=L89.303^^30^385^239
 ;;^UTILITY(U,$J,358.3,6008,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6008,1,3,0)
 ;;=3^Pressure Ulcer of Buttock,Stage 3
 ;;^UTILITY(U,$J,358.3,6008,1,4,0)
 ;;=4^L89.303
 ;;^UTILITY(U,$J,358.3,6008,2)
 ;;=^5009392
 ;;^UTILITY(U,$J,358.3,6009,0)
 ;;=L89.304^^30^385^240
 ;;^UTILITY(U,$J,358.3,6009,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6009,1,3,0)
 ;;=3^Pressure Ulcer of Buttock,Stage 4
 ;;^UTILITY(U,$J,358.3,6009,1,4,0)
 ;;=4^L89.304
 ;;^UTILITY(U,$J,358.3,6009,2)
 ;;=^5009393
 ;;^UTILITY(U,$J,358.3,6010,0)
 ;;=L89.309^^30^385^241
 ;;^UTILITY(U,$J,358.3,6010,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6010,1,3,0)
 ;;=3^Pressure Ulcer of Buttock,Stage Unspec
 ;;^UTILITY(U,$J,358.3,6010,1,4,0)
 ;;=4^L89.309
 ;;^UTILITY(U,$J,358.3,6010,2)
 ;;=^5133672
 ;;^UTILITY(U,$J,358.3,6011,0)
 ;;=L89.90^^30^385^247
 ;;^UTILITY(U,$J,358.3,6011,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6011,1,3,0)
 ;;=3^Pressure Ulcer of Unspec Site,Unspec Stage
 ;;^UTILITY(U,$J,358.3,6011,1,4,0)
 ;;=4^L89.90
 ;;^UTILITY(U,$J,358.3,6011,2)
 ;;=^5133666
 ;;^UTILITY(U,$J,358.3,6012,0)
 ;;=L89.91^^30^385^243
 ;;^UTILITY(U,$J,358.3,6012,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6012,1,3,0)
 ;;=3^Pressure Ulcer of Unspec Site,Stage 1
 ;;^UTILITY(U,$J,358.3,6012,1,4,0)
 ;;=4^L89.91
 ;;^UTILITY(U,$J,358.3,6012,2)
 ;;=^5133664
 ;;^UTILITY(U,$J,358.3,6013,0)
 ;;=L89.92^^30^385^244
 ;;^UTILITY(U,$J,358.3,6013,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6013,1,3,0)
 ;;=3^Pressure Ulcer of Unspec Site,Stage 2
 ;;^UTILITY(U,$J,358.3,6013,1,4,0)
 ;;=4^L89.92
 ;;^UTILITY(U,$J,358.3,6013,2)
 ;;=^5133667
 ;;^UTILITY(U,$J,358.3,6014,0)
 ;;=L89.93^^30^385^245
 ;;^UTILITY(U,$J,358.3,6014,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6014,1,3,0)
 ;;=3^Pressure Ulcer of Unspec Site,Stage 3
 ;;^UTILITY(U,$J,358.3,6014,1,4,0)
 ;;=4^L89.93
 ;;^UTILITY(U,$J,358.3,6014,2)
 ;;=^5133668
 ;;^UTILITY(U,$J,358.3,6015,0)
 ;;=L89.94^^30^385^246
 ;;^UTILITY(U,$J,358.3,6015,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6015,1,3,0)
 ;;=3^Pressure Ulcer of Unspec Site,Stage 4
 ;;^UTILITY(U,$J,358.3,6015,1,4,0)
 ;;=4^L89.94
 ;;^UTILITY(U,$J,358.3,6015,2)
 ;;=^5133669
 ;;^UTILITY(U,$J,358.3,6016,0)
 ;;=L89.95^^30^385^248
 ;;^UTILITY(U,$J,358.3,6016,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6016,1,3,0)
 ;;=3^Pressure Ulcer of Unspec Site,Unstageable
 ;;^UTILITY(U,$J,358.3,6016,1,4,0)
 ;;=4^L89.95
 ;;^UTILITY(U,$J,358.3,6016,2)
 ;;=^5133660
 ;;^UTILITY(U,$J,358.3,6017,0)
 ;;=L92.0^^30^385^156
 ;;^UTILITY(U,$J,358.3,6017,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6017,1,3,0)
 ;;=3^Granuloma Annulare
 ;;^UTILITY(U,$J,358.3,6017,1,4,0)
 ;;=4^L92.0
 ;;^UTILITY(U,$J,358.3,6017,2)
 ;;=^184052
 ;;^UTILITY(U,$J,358.3,6018,0)
 ;;=L95.1^^30^385^143
 ;;^UTILITY(U,$J,358.3,6018,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,6018,1,3,0)
 ;;=3^Erythema Elevatum Diutinum
 ;;^UTILITY(U,$J,358.3,6018,1,4,0)
 ;;=4^L95.1
 ;;^UTILITY(U,$J,358.3,6018,2)
 ;;=^5009477
 ;;^UTILITY(U,$J,358.3,6019,0)
 ;;=L97.111^^30^385^226
