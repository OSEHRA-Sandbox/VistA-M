IBDEI1V5 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,31186,1,4,0)
 ;;=4^W19.XXXD
 ;;^UTILITY(U,$J,358.3,31186,2)
 ;;=^5059834
 ;;^UTILITY(U,$J,358.3,31187,0)
 ;;=W54.0XXA^^135^1391^11
 ;;^UTILITY(U,$J,358.3,31187,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31187,1,3,0)
 ;;=3^Bitten by Dog,Init Encntr
 ;;^UTILITY(U,$J,358.3,31187,1,4,0)
 ;;=4^W54.0XXA
 ;;^UTILITY(U,$J,358.3,31187,2)
 ;;=^5060256
 ;;^UTILITY(U,$J,358.3,31188,0)
 ;;=W54.0XXD^^135^1391^12
 ;;^UTILITY(U,$J,358.3,31188,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31188,1,3,0)
 ;;=3^Bitten by Dog,Subs Encntr
 ;;^UTILITY(U,$J,358.3,31188,1,4,0)
 ;;=4^W54.0XXD
 ;;^UTILITY(U,$J,358.3,31188,2)
 ;;=^5060257
 ;;^UTILITY(U,$J,358.3,31189,0)
 ;;=W55.01XA^^135^1391^9
 ;;^UTILITY(U,$J,358.3,31189,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31189,1,3,0)
 ;;=3^Bitten by Cat,Init Encntr
 ;;^UTILITY(U,$J,358.3,31189,1,4,0)
 ;;=4^W55.01XA
 ;;^UTILITY(U,$J,358.3,31189,2)
 ;;=^5060265
 ;;^UTILITY(U,$J,358.3,31190,0)
 ;;=W55.01XD^^135^1391^10
 ;;^UTILITY(U,$J,358.3,31190,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31190,1,3,0)
 ;;=3^Bitten by Cat,Subs Encntr
 ;;^UTILITY(U,$J,358.3,31190,1,4,0)
 ;;=4^W55.01XD
 ;;^UTILITY(U,$J,358.3,31190,2)
 ;;=^5060266
 ;;^UTILITY(U,$J,358.3,31191,0)
 ;;=W55.03XA^^135^1391^101
 ;;^UTILITY(U,$J,358.3,31191,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31191,1,3,0)
 ;;=3^Scratched by Cat,Init Encntr
 ;;^UTILITY(U,$J,358.3,31191,1,4,0)
 ;;=4^W55.03XA
 ;;^UTILITY(U,$J,358.3,31191,2)
 ;;=^5060268
 ;;^UTILITY(U,$J,358.3,31192,0)
 ;;=W55.03XD^^135^1391^102
 ;;^UTILITY(U,$J,358.3,31192,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31192,1,3,0)
 ;;=3^Scratched by Cat,Subs Encntr
 ;;^UTILITY(U,$J,358.3,31192,1,4,0)
 ;;=4^W55.03XD
 ;;^UTILITY(U,$J,358.3,31192,2)
 ;;=^5060269
 ;;^UTILITY(U,$J,358.3,31193,0)
 ;;=X00.8XXA^^135^1391^17
 ;;^UTILITY(U,$J,358.3,31193,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31193,1,3,0)
 ;;=3^Exp to Uncontrolled Bldg Fire,Init Encntr
 ;;^UTILITY(U,$J,358.3,31193,1,4,0)
 ;;=4^X00.8XXA
 ;;^UTILITY(U,$J,358.3,31193,2)
 ;;=^5060679
 ;;^UTILITY(U,$J,358.3,31194,0)
 ;;=X00.8XXD^^135^1391^18
 ;;^UTILITY(U,$J,358.3,31194,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31194,1,3,0)
 ;;=3^Exp to Uncontrolled Bldg Fire,Subs Encntr
 ;;^UTILITY(U,$J,358.3,31194,1,4,0)
 ;;=4^X00.8XXD
 ;;^UTILITY(U,$J,358.3,31194,2)
 ;;=^5060680
 ;;^UTILITY(U,$J,358.3,31195,0)
 ;;=X32.XXXA^^135^1391^15
 ;;^UTILITY(U,$J,358.3,31195,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31195,1,3,0)
 ;;=3^Exp to Sunlight,Init Encntr
 ;;^UTILITY(U,$J,358.3,31195,1,4,0)
 ;;=4^X32.XXXA
 ;;^UTILITY(U,$J,358.3,31195,2)
 ;;=^5060847
 ;;^UTILITY(U,$J,358.3,31196,0)
 ;;=X32.XXXD^^135^1391^16
 ;;^UTILITY(U,$J,358.3,31196,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31196,1,3,0)
 ;;=3^Exp to Sunlight,Subs Encntr
 ;;^UTILITY(U,$J,358.3,31196,1,4,0)
 ;;=4^X32.XXXD
 ;;^UTILITY(U,$J,358.3,31196,2)
 ;;=^5060848
 ;;^UTILITY(U,$J,358.3,31197,0)
 ;;=Y04.0XXA^^135^1391^7
 ;;^UTILITY(U,$J,358.3,31197,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31197,1,3,0)
 ;;=3^Assault by Unarmed Fight,Init Encntr
 ;;^UTILITY(U,$J,358.3,31197,1,4,0)
 ;;=4^Y04.0XXA
 ;;^UTILITY(U,$J,358.3,31197,2)
 ;;=^5061165
 ;;^UTILITY(U,$J,358.3,31198,0)
 ;;=Y04.0XXD^^135^1391^8
 ;;^UTILITY(U,$J,358.3,31198,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31198,1,3,0)
 ;;=3^Assault by Unarmed Fight,Subs Encntr
 ;;^UTILITY(U,$J,358.3,31198,1,4,0)
 ;;=4^Y04.0XXD
 ;;^UTILITY(U,$J,358.3,31198,2)
 ;;=^5061166
 ;;^UTILITY(U,$J,358.3,31199,0)
 ;;=Y04.1XXA^^135^1391^1
 ;;^UTILITY(U,$J,358.3,31199,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,31199,1,3,0)
 ;;=3^Assault by Human Bite,Init Encntr
