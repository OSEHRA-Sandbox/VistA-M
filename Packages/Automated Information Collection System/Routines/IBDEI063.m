IBDEI063 ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,2512,2)
 ;;=^5013322
 ;;^UTILITY(U,$J,358.3,2513,0)
 ;;=M25.50^^15^192^9
 ;;^UTILITY(U,$J,358.3,2513,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2513,1,3,0)
 ;;=3^Pain in unspecified joint
 ;;^UTILITY(U,$J,358.3,2513,1,4,0)
 ;;=4^M25.50
 ;;^UTILITY(U,$J,358.3,2513,2)
 ;;=^5011601
 ;;^UTILITY(U,$J,358.3,2514,0)
 ;;=M96.1^^15^192^10
 ;;^UTILITY(U,$J,358.3,2514,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2514,1,3,0)
 ;;=3^Postlaminectomy syndrome NEC
 ;;^UTILITY(U,$J,358.3,2514,1,4,0)
 ;;=4^M96.1
 ;;^UTILITY(U,$J,358.3,2514,2)
 ;;=^5015374
 ;;^UTILITY(U,$J,358.3,2515,0)
 ;;=M54.10^^15^192^11
 ;;^UTILITY(U,$J,358.3,2515,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2515,1,3,0)
 ;;=3^Radiculopathy, site unspecified
 ;;^UTILITY(U,$J,358.3,2515,1,4,0)
 ;;=4^M54.10
 ;;^UTILITY(U,$J,358.3,2515,2)
 ;;=^5012295
 ;;^UTILITY(U,$J,358.3,2516,0)
 ;;=M47.10^^15^192^13
 ;;^UTILITY(U,$J,358.3,2516,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2516,1,3,0)
 ;;=3^Spondylosis w/ myelopathy, other, site unspec
 ;;^UTILITY(U,$J,358.3,2516,1,4,0)
 ;;=4^M47.10
 ;;^UTILITY(U,$J,358.3,2516,2)
 ;;=^5012050
 ;;^UTILITY(U,$J,358.3,2517,0)
 ;;=M43.10^^15^192^12
 ;;^UTILITY(U,$J,358.3,2517,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2517,1,3,0)
 ;;=3^Spondylolisthesis, site unspecified
 ;;^UTILITY(U,$J,358.3,2517,1,4,0)
 ;;=4^M43.10
 ;;^UTILITY(U,$J,358.3,2517,2)
 ;;=^5011921
 ;;^UTILITY(U,$J,358.3,2518,0)
 ;;=M79.1^^15^192^7
 ;;^UTILITY(U,$J,358.3,2518,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2518,1,3,0)
 ;;=3^Myalgia
 ;;^UTILITY(U,$J,358.3,2518,1,4,0)
 ;;=4^M79.1
 ;;^UTILITY(U,$J,358.3,2518,2)
 ;;=^5013321
 ;;^UTILITY(U,$J,358.3,2519,0)
 ;;=M79.7^^15^192^4
 ;;^UTILITY(U,$J,358.3,2519,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,2519,1,3,0)
 ;;=3^Fibromyalgia
 ;;^UTILITY(U,$J,358.3,2519,1,4,0)
 ;;=4^M79.7
 ;;^UTILITY(U,$J,358.3,2519,2)
 ;;=^46261
 ;;^UTILITY(U,$J,358.3,2520,0)
 ;;=99307^^16^193^1
 ;;^UTILITY(U,$J,358.3,2520,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2520,1,1,0)
 ;;=1^Prob Focus-Daily
 ;;^UTILITY(U,$J,358.3,2520,1,2,0)
 ;;=2^99307
 ;;^UTILITY(U,$J,358.3,2521,0)
 ;;=99308^^16^193^2
 ;;^UTILITY(U,$J,358.3,2521,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2521,1,1,0)
 ;;=1^Exp Prob Focus-Daily
 ;;^UTILITY(U,$J,358.3,2521,1,2,0)
 ;;=2^99308
 ;;^UTILITY(U,$J,358.3,2522,0)
 ;;=99309^^16^193^3
 ;;^UTILITY(U,$J,358.3,2522,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2522,1,1,0)
 ;;=1^Detailed-Daily
 ;;^UTILITY(U,$J,358.3,2522,1,2,0)
 ;;=2^99309
 ;;^UTILITY(U,$J,358.3,2523,0)
 ;;=99310^^16^193^4
 ;;^UTILITY(U,$J,358.3,2523,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2523,1,1,0)
 ;;=1^Comprehensive-Daily
 ;;^UTILITY(U,$J,358.3,2523,1,2,0)
 ;;=2^99310
 ;;^UTILITY(U,$J,358.3,2524,0)
 ;;=99304^^16^194^1
 ;;^UTILITY(U,$J,358.3,2524,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2524,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,2524,1,2,0)
 ;;=2^99304
 ;;^UTILITY(U,$J,358.3,2525,0)
 ;;=99305^^16^194^2
 ;;^UTILITY(U,$J,358.3,2525,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2525,1,1,0)
 ;;=1^Comprehensive/Moderate
 ;;^UTILITY(U,$J,358.3,2525,1,2,0)
 ;;=2^99305
 ;;^UTILITY(U,$J,358.3,2526,0)
 ;;=99306^^16^194^3
 ;;^UTILITY(U,$J,358.3,2526,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2526,1,1,0)
 ;;=1^Comprehensive/High
 ;;^UTILITY(U,$J,358.3,2526,1,2,0)
 ;;=2^99306
 ;;^UTILITY(U,$J,358.3,2527,0)
 ;;=99251^^16^195^1
 ;;^UTILITY(U,$J,358.3,2527,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,2527,1,1,0)
 ;;=1^Prob Focus
 ;;^UTILITY(U,$J,358.3,2527,1,2,0)
 ;;=2^99251
 ;;^UTILITY(U,$J,358.3,2528,0)
 ;;=99252^^16^195^2
