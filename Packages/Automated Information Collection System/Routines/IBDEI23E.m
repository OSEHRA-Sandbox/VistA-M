IBDEI23E ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,35113,1,4,0)
 ;;=4^R53.81
 ;;^UTILITY(U,$J,358.3,35113,2)
 ;;=^5019518
 ;;^UTILITY(U,$J,358.3,35114,0)
 ;;=R53.83^^163^1790^18
 ;;^UTILITY(U,$J,358.3,35114,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35114,1,3,0)
 ;;=3^Fatigue NOS
 ;;^UTILITY(U,$J,358.3,35114,1,4,0)
 ;;=4^R53.83
 ;;^UTILITY(U,$J,358.3,35114,2)
 ;;=^5019520
 ;;^UTILITY(U,$J,358.3,35115,0)
 ;;=G93.3^^163^1790^30
 ;;^UTILITY(U,$J,358.3,35115,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35115,1,3,0)
 ;;=3^Postviral Fatigue Syndrome
 ;;^UTILITY(U,$J,358.3,35115,1,4,0)
 ;;=4^G93.3
 ;;^UTILITY(U,$J,358.3,35115,2)
 ;;=^5004181
 ;;^UTILITY(U,$J,358.3,35116,0)
 ;;=R53.1^^163^1790^38
 ;;^UTILITY(U,$J,358.3,35116,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35116,1,3,0)
 ;;=3^Weakness
 ;;^UTILITY(U,$J,358.3,35116,1,4,0)
 ;;=4^R53.1
 ;;^UTILITY(U,$J,358.3,35116,2)
 ;;=^5019516
 ;;^UTILITY(U,$J,358.3,35117,0)
 ;;=R60.9^^163^1790^16
 ;;^UTILITY(U,$J,358.3,35117,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35117,1,3,0)
 ;;=3^Edema,Unspec
 ;;^UTILITY(U,$J,358.3,35117,1,4,0)
 ;;=4^R60.9
 ;;^UTILITY(U,$J,358.3,35117,2)
 ;;=^5019534
 ;;^UTILITY(U,$J,358.3,35118,0)
 ;;=R60.1^^163^1790^14
 ;;^UTILITY(U,$J,358.3,35118,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35118,1,3,0)
 ;;=3^Edema,Generalized
 ;;^UTILITY(U,$J,358.3,35118,1,4,0)
 ;;=4^R60.1
 ;;^UTILITY(U,$J,358.3,35118,2)
 ;;=^5019533
 ;;^UTILITY(U,$J,358.3,35119,0)
 ;;=R60.0^^163^1790^15
 ;;^UTILITY(U,$J,358.3,35119,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35119,1,3,0)
 ;;=3^Edema,Localized
 ;;^UTILITY(U,$J,358.3,35119,1,4,0)
 ;;=4^R60.0
 ;;^UTILITY(U,$J,358.3,35119,2)
 ;;=^5019532
 ;;^UTILITY(U,$J,358.3,35120,0)
 ;;=R63.4^^163^1790^6
 ;;^UTILITY(U,$J,358.3,35120,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35120,1,3,0)
 ;;=3^Abnormal Weight Loss
 ;;^UTILITY(U,$J,358.3,35120,1,4,0)
 ;;=4^R63.4
 ;;^UTILITY(U,$J,358.3,35120,2)
 ;;=^5019542
 ;;^UTILITY(U,$J,358.3,35121,0)
 ;;=R63.6^^163^1790^35
 ;;^UTILITY(U,$J,358.3,35121,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35121,1,3,0)
 ;;=3^Underweight
 ;;^UTILITY(U,$J,358.3,35121,1,4,0)
 ;;=4^R63.6
 ;;^UTILITY(U,$J,358.3,35121,2)
 ;;=^322007
 ;;^UTILITY(U,$J,358.3,35122,0)
 ;;=R51.^^163^1790^20
 ;;^UTILITY(U,$J,358.3,35122,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35122,1,3,0)
 ;;=3^Headache
 ;;^UTILITY(U,$J,358.3,35122,1,4,0)
 ;;=4^R51.
 ;;^UTILITY(U,$J,358.3,35122,2)
 ;;=^5019513
 ;;^UTILITY(U,$J,358.3,35123,0)
 ;;=R05.^^163^1790^12
 ;;^UTILITY(U,$J,358.3,35123,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35123,1,3,0)
 ;;=3^Cough
 ;;^UTILITY(U,$J,358.3,35123,1,4,0)
 ;;=4^R05.
 ;;^UTILITY(U,$J,358.3,35123,2)
 ;;=^5019179
 ;;^UTILITY(U,$J,358.3,35124,0)
 ;;=R07.9^^163^1790^9
 ;;^UTILITY(U,$J,358.3,35124,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35124,1,3,0)
 ;;=3^Chest Pain,Unspec
 ;;^UTILITY(U,$J,358.3,35124,1,4,0)
 ;;=4^R07.9
 ;;^UTILITY(U,$J,358.3,35124,2)
 ;;=^5019201
 ;;^UTILITY(U,$J,358.3,35125,0)
 ;;=R10.9^^163^1790^5
 ;;^UTILITY(U,$J,358.3,35125,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35125,1,3,0)
 ;;=3^Abdominal Pain,Unspec
 ;;^UTILITY(U,$J,358.3,35125,1,4,0)
 ;;=4^R10.9
 ;;^UTILITY(U,$J,358.3,35125,2)
 ;;=^5019230
 ;;^UTILITY(U,$J,358.3,35126,0)
 ;;=R10.11^^163^1790^4
 ;;^UTILITY(U,$J,358.3,35126,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35126,1,3,0)
 ;;=3^Abdominal Pain,RUQ
 ;;^UTILITY(U,$J,358.3,35126,1,4,0)
 ;;=4^R10.11
 ;;^UTILITY(U,$J,358.3,35126,2)
 ;;=^5019206
 ;;^UTILITY(U,$J,358.3,35127,0)
 ;;=R10.12^^163^1790^2
 ;;^UTILITY(U,$J,358.3,35127,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,35127,1,3,0)
 ;;=3^Abdominal Pain,LUQ
