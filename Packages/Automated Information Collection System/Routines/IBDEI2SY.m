IBDEI2SY ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,47042,2)
 ;;=^5011865
 ;;^UTILITY(U,$J,358.3,47043,0)
 ;;=R10.9^^206^2317^1
 ;;^UTILITY(U,$J,358.3,47043,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47043,1,3,0)
 ;;=3^Abdominal Pain,Unspec
 ;;^UTILITY(U,$J,358.3,47043,1,4,0)
 ;;=4^R10.9
 ;;^UTILITY(U,$J,358.3,47043,2)
 ;;=^5019230
 ;;^UTILITY(U,$J,358.3,47044,0)
 ;;=N39.41^^206^2317^73
 ;;^UTILITY(U,$J,358.3,47044,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47044,1,3,0)
 ;;=3^Urge Incontinence
 ;;^UTILITY(U,$J,358.3,47044,1,4,0)
 ;;=4^N39.41
 ;;^UTILITY(U,$J,358.3,47044,2)
 ;;=^5015680
 ;;^UTILITY(U,$J,358.3,47045,0)
 ;;=G44.1^^206^2317^74
 ;;^UTILITY(U,$J,358.3,47045,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47045,1,3,0)
 ;;=3^Vascular Headache NEC
 ;;^UTILITY(U,$J,358.3,47045,1,4,0)
 ;;=4^G44.1
 ;;^UTILITY(U,$J,358.3,47045,2)
 ;;=^5003934
 ;;^UTILITY(U,$J,358.3,47046,0)
 ;;=R53.1^^206^2317^75
 ;;^UTILITY(U,$J,358.3,47046,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47046,1,3,0)
 ;;=3^Weakness
 ;;^UTILITY(U,$J,358.3,47046,1,4,0)
 ;;=4^R53.1
 ;;^UTILITY(U,$J,358.3,47046,2)
 ;;=^5019516
 ;;^UTILITY(U,$J,358.3,47047,0)
 ;;=B02.9^^206^2317^76
 ;;^UTILITY(U,$J,358.3,47047,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47047,1,3,0)
 ;;=3^Zoster w/o Complications
 ;;^UTILITY(U,$J,358.3,47047,1,4,0)
 ;;=4^B02.9
 ;;^UTILITY(U,$J,358.3,47047,2)
 ;;=^5000501
 ;;^UTILITY(U,$J,358.3,47048,0)
 ;;=99212^^207^2318^2
 ;;^UTILITY(U,$J,358.3,47048,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47048,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,47048,1,4,0)
 ;;=4^Problem Focused
 ;;^UTILITY(U,$J,358.3,47049,0)
 ;;=99213^^207^2318^3
 ;;^UTILITY(U,$J,358.3,47049,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47049,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,47049,1,4,0)
 ;;=4^Expanded Problem Focused
 ;;^UTILITY(U,$J,358.3,47050,0)
 ;;=99214^^207^2318^4
 ;;^UTILITY(U,$J,358.3,47050,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47050,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,47050,1,4,0)
 ;;=4^Detailed
 ;;^UTILITY(U,$J,358.3,47051,0)
 ;;=99215^^207^2318^5
 ;;^UTILITY(U,$J,358.3,47051,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47051,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,47051,1,4,0)
 ;;=4^Comprehensive
 ;;^UTILITY(U,$J,358.3,47052,0)
 ;;=99024^^207^2318^6
 ;;^UTILITY(U,$J,358.3,47052,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47052,1,2,0)
 ;;=2^99024
 ;;^UTILITY(U,$J,358.3,47052,1,4,0)
 ;;=4^Follow-up within 90 days
 ;;^UTILITY(U,$J,358.3,47053,0)
 ;;=99211^^207^2318^1
 ;;^UTILITY(U,$J,358.3,47053,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47053,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,47053,1,4,0)
 ;;=4^Brief Visit
 ;;^UTILITY(U,$J,358.3,47054,0)
 ;;=99241^^207^2319^1
 ;;^UTILITY(U,$J,358.3,47054,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47054,1,2,0)
 ;;=2^99241
 ;;^UTILITY(U,$J,358.3,47054,1,4,0)
 ;;=4^Problem Focused
 ;;^UTILITY(U,$J,358.3,47055,0)
 ;;=99242^^207^2319^2
 ;;^UTILITY(U,$J,358.3,47055,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47055,1,2,0)
 ;;=2^99242
 ;;^UTILITY(U,$J,358.3,47055,1,4,0)
 ;;=4^Expanded Problem Focused
 ;;^UTILITY(U,$J,358.3,47056,0)
 ;;=99243^^207^2319^3
 ;;^UTILITY(U,$J,358.3,47056,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47056,1,2,0)
 ;;=2^99243
 ;;^UTILITY(U,$J,358.3,47056,1,4,0)
 ;;=4^Detailed
 ;;^UTILITY(U,$J,358.3,47057,0)
 ;;=99244^^207^2319^4
 ;;^UTILITY(U,$J,358.3,47057,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47057,1,2,0)
 ;;=2^99244
 ;;^UTILITY(U,$J,358.3,47057,1,4,0)
 ;;=4^Comprehensive
 ;;^UTILITY(U,$J,358.3,47058,0)
 ;;=99245^^207^2319^5
 ;;^UTILITY(U,$J,358.3,47058,1,0)
 ;;=^358.31IA^4^2
