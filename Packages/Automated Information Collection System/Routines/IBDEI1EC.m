IBDEI1EC ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,25046,0)
 ;;=V65.43^^163^1619^4
 ;;^UTILITY(U,$J,358.3,25046,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,25046,1,2,0)
 ;;=2^V65.43
 ;;^UTILITY(U,$J,358.3,25046,1,5,0)
 ;;=5^COUNSEL-INJURY PREVEN
 ;;^UTILITY(U,$J,358.3,25046,2)
 ;;=^303468
 ;;^UTILITY(U,$J,358.3,25047,0)
 ;;=V65.45^^163^1619^5
 ;;^UTILITY(U,$J,358.3,25047,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,25047,1,2,0)
 ;;=2^V65.45
 ;;^UTILITY(U,$J,358.3,25047,1,5,0)
 ;;=5^COUNSEL-OTH SEX TX DISEASES
 ;;^UTILITY(U,$J,358.3,25047,2)
 ;;=^303470
 ;;^UTILITY(U,$J,358.3,25048,0)
 ;;=V71.09^^163^1620^1
 ;;^UTILITY(U,$J,358.3,25048,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,25048,1,2,0)
 ;;=2^V71.09
 ;;^UTILITY(U,$J,358.3,25048,1,5,0)
 ;;=5^OBSERV-MENTAL COND NEC
 ;;^UTILITY(U,$J,358.3,25048,2)
 ;;=^295604
 ;;^UTILITY(U,$J,358.3,25049,0)
 ;;=99211^^164^1621^1
 ;;^UTILITY(U,$J,358.3,25049,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25049,1,1,0)
 ;;=1^Brief Office Visit
 ;;^UTILITY(U,$J,358.3,25049,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,25050,0)
 ;;=99212^^164^1621^2
 ;;^UTILITY(U,$J,358.3,25050,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25050,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,25050,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,25051,0)
 ;;=99213^^164^1621^3
 ;;^UTILITY(U,$J,358.3,25051,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25051,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,25051,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,25052,0)
 ;;=99214^^164^1621^4
 ;;^UTILITY(U,$J,358.3,25052,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25052,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,25052,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,25053,0)
 ;;=99215^^164^1621^5
 ;;^UTILITY(U,$J,358.3,25053,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25053,1,1,0)
 ;;=1^Comprehensive
 ;;^UTILITY(U,$J,358.3,25053,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,25054,0)
 ;;=99201^^164^1622^1
 ;;^UTILITY(U,$J,358.3,25054,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25054,1,1,0)
 ;;=1^Problem Focus
 ;;^UTILITY(U,$J,358.3,25054,1,2,0)
 ;;=2^99201
 ;;^UTILITY(U,$J,358.3,25055,0)
 ;;=99202^^164^1622^2
 ;;^UTILITY(U,$J,358.3,25055,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25055,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,25055,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,25056,0)
 ;;=99203^^164^1622^3
 ;;^UTILITY(U,$J,358.3,25056,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25056,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,25056,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,25057,0)
 ;;=99204^^164^1622^4
 ;;^UTILITY(U,$J,358.3,25057,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25057,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,25057,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,25058,0)
 ;;=99205^^164^1622^5
 ;;^UTILITY(U,$J,358.3,25058,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25058,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,25058,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,25059,0)
 ;;=99211^^164^1623^1
 ;;^UTILITY(U,$J,358.3,25059,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25059,1,1,0)
 ;;=1^Brief Visit w/ mod
 ;;^UTILITY(U,$J,358.3,25059,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,25059,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,25059,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,25060,0)
 ;;=99212^^164^1623^2
 ;;^UTILITY(U,$J,358.3,25060,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,25060,1,1,0)
 ;;=1^Problem Focused w/ mod
 ;;^UTILITY(U,$J,358.3,25060,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,25060,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,25060,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,25061,0)
 ;;=99213^^164^1623^3
