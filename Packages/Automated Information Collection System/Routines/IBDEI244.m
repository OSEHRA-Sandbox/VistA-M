IBDEI244 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,35475,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,35476,0)
 ;;=99203^^165^1817^3
 ;;^UTILITY(U,$J,358.3,35476,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35476,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,35476,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,35477,0)
 ;;=99204^^165^1817^4
 ;;^UTILITY(U,$J,358.3,35477,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35477,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,35477,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,35478,0)
 ;;=99205^^165^1817^5
 ;;^UTILITY(U,$J,358.3,35478,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35478,1,1,0)
 ;;=1^Comprehensive,High
 ;;^UTILITY(U,$J,358.3,35478,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,35479,0)
 ;;=99211^^165^1818^1
 ;;^UTILITY(U,$J,358.3,35479,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35479,1,1,0)
 ;;=1^Nurse Visit (no MD seen)
 ;;^UTILITY(U,$J,358.3,35479,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,35480,0)
 ;;=99212^^165^1818^2
 ;;^UTILITY(U,$J,358.3,35480,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35480,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,35480,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,35481,0)
 ;;=99213^^165^1818^3
 ;;^UTILITY(U,$J,358.3,35481,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35481,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,35481,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,35482,0)
 ;;=99214^^165^1818^4
 ;;^UTILITY(U,$J,358.3,35482,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35482,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,35482,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,35483,0)
 ;;=99215^^165^1818^5
 ;;^UTILITY(U,$J,358.3,35483,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35483,1,1,0)
 ;;=1^Comprehensive
 ;;^UTILITY(U,$J,358.3,35483,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,35484,0)
 ;;=99024^^165^1818^6
 ;;^UTILITY(U,$J,358.3,35484,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35484,1,1,0)
 ;;=1^Post Op Visit in Global
 ;;^UTILITY(U,$J,358.3,35484,1,2,0)
 ;;=2^99024
 ;;^UTILITY(U,$J,358.3,35485,0)
 ;;=99241^^165^1819^1
 ;;^UTILITY(U,$J,358.3,35485,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35485,1,1,0)
 ;;=1^Prob Focused Exam
 ;;^UTILITY(U,$J,358.3,35485,1,2,0)
 ;;=2^99241
 ;;^UTILITY(U,$J,358.3,35486,0)
 ;;=99242^^165^1819^2
 ;;^UTILITY(U,$J,358.3,35486,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35486,1,1,0)
 ;;=1^Exp Prob Focused Exam
 ;;^UTILITY(U,$J,358.3,35486,1,2,0)
 ;;=2^99242
 ;;^UTILITY(U,$J,358.3,35487,0)
 ;;=99243^^165^1819^3
 ;;^UTILITY(U,$J,358.3,35487,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35487,1,1,0)
 ;;=1^Detailed Exam
 ;;^UTILITY(U,$J,358.3,35487,1,2,0)
 ;;=2^99243
 ;;^UTILITY(U,$J,358.3,35488,0)
 ;;=99244^^165^1819^4
 ;;^UTILITY(U,$J,358.3,35488,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35488,1,1,0)
 ;;=1^Comprehensive,Moderate
 ;;^UTILITY(U,$J,358.3,35488,1,2,0)
 ;;=2^99244
 ;;^UTILITY(U,$J,358.3,35489,0)
 ;;=99245^^165^1819^5
 ;;^UTILITY(U,$J,358.3,35489,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,35489,1,1,0)
 ;;=1^Comprehensive,High
 ;;^UTILITY(U,$J,358.3,35489,1,2,0)
 ;;=2^99245
 ;;^UTILITY(U,$J,358.3,35490,0)
 ;;=O01.9^^166^1820^16
 ;;^UTILITY(U,$J,358.3,35490,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35490,1,3,0)
 ;;=3^Hydatidiform mole, unspecified
 ;;^UTILITY(U,$J,358.3,35490,1,4,0)
 ;;=4^O01.9
 ;;^UTILITY(U,$J,358.3,35490,2)
 ;;=^5015977
 ;;^UTILITY(U,$J,358.3,35491,0)
 ;;=O02.81^^166^1820^17
 ;;^UTILITY(U,$J,358.3,35491,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35491,1,3,0)
 ;;=3^Inapprop chg quantitav hCG in early pregnancy
 ;;^UTILITY(U,$J,358.3,35491,1,4,0)
 ;;=4^O02.81
