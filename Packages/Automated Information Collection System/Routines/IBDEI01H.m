IBDEI01H ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,206,1,5,0)
 ;;=5^Opioid + Other Depend
 ;;^UTILITY(U,$J,358.3,206,2)
 ;;=^268214
 ;;^UTILITY(U,$J,358.3,207,0)
 ;;=304.73^^2^18^64
 ;;^UTILITY(U,$J,358.3,207,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,207,1,2,0)
 ;;=2^304.73
 ;;^UTILITY(U,$J,358.3,207,1,5,0)
 ;;=5^Opioid + Other Dep-Remis
 ;;^UTILITY(U,$J,358.3,207,2)
 ;;=^268217
 ;;^UTILITY(U,$J,358.3,208,0)
 ;;=304.80^^2^18^50
 ;;^UTILITY(U,$J,358.3,208,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,208,1,2,0)
 ;;=2^304.80
 ;;^UTILITY(U,$J,358.3,208,1,5,0)
 ;;=5^Combination Drug Dep
 ;;^UTILITY(U,$J,358.3,208,2)
 ;;=^268218
 ;;^UTILITY(U,$J,358.3,209,0)
 ;;=304.83^^2^18^47
 ;;^UTILITY(U,$J,358.3,209,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,209,1,2,0)
 ;;=2^304.83
 ;;^UTILITY(U,$J,358.3,209,1,5,0)
 ;;=5^Comb Drug Dep-Remission
 ;;^UTILITY(U,$J,358.3,209,2)
 ;;=^268221
 ;;^UTILITY(U,$J,358.3,210,0)
 ;;=305.1^^2^18^63
 ;;^UTILITY(U,$J,358.3,210,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,210,1,2,0)
 ;;=2^305.1
 ;;^UTILITY(U,$J,358.3,210,1,5,0)
 ;;=5^Nicotine Dependence
 ;;^UTILITY(U,$J,358.3,210,2)
 ;;=^119899
 ;;^UTILITY(U,$J,358.3,211,0)
 ;;=291.81^^2^18^15
 ;;^UTILITY(U,$J,358.3,211,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,211,1,2,0)
 ;;=2^291.81
 ;;^UTILITY(U,$J,358.3,211,1,5,0)
 ;;=5^Alcohol Withdrawal
 ;;^UTILITY(U,$J,358.3,211,2)
 ;;=^123498
 ;;^UTILITY(U,$J,358.3,212,0)
 ;;=291.0^^2^18^14
 ;;^UTILITY(U,$J,358.3,212,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,212,1,2,0)
 ;;=2^291.0
 ;;^UTILITY(U,$J,358.3,212,1,5,0)
 ;;=5^Alcohol Withdraw Delir
 ;;^UTILITY(U,$J,358.3,212,2)
 ;;=^4589
 ;;^UTILITY(U,$J,358.3,213,0)
 ;;=303.00^^2^18^5
 ;;^UTILITY(U,$J,358.3,213,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,213,1,2,0)
 ;;=2^303.00
 ;;^UTILITY(U,$J,358.3,213,1,5,0)
 ;;=5^Alc Intox NOS
 ;;^UTILITY(U,$J,358.3,213,2)
 ;;=^268183
 ;;^UTILITY(U,$J,358.3,214,0)
 ;;=303.01^^2^18^6
 ;;^UTILITY(U,$J,358.3,214,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,214,1,2,0)
 ;;=2^303.01
 ;;^UTILITY(U,$J,358.3,214,1,5,0)
 ;;=5^Alc Intox, Continuous
 ;;^UTILITY(U,$J,358.3,214,2)
 ;;=^268184
 ;;^UTILITY(U,$J,358.3,215,0)
 ;;=303.02^^2^18^7
 ;;^UTILITY(U,$J,358.3,215,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,215,1,2,0)
 ;;=2^303.02
 ;;^UTILITY(U,$J,358.3,215,1,5,0)
 ;;=5^Alc Intox, Episodic
 ;;^UTILITY(U,$J,358.3,215,2)
 ;;=^268185
 ;;^UTILITY(U,$J,358.3,216,0)
 ;;=303.03^^2^18^8
 ;;^UTILITY(U,$J,358.3,216,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,216,1,2,0)
 ;;=2^303.03
 ;;^UTILITY(U,$J,358.3,216,1,5,0)
 ;;=5^Alc Intox, Remiss
 ;;^UTILITY(U,$J,358.3,216,2)
 ;;=^268186
 ;;^UTILITY(U,$J,358.3,217,0)
 ;;=303.91^^2^18^3
 ;;^UTILITY(U,$J,358.3,217,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,217,1,2,0)
 ;;=2^303.91
 ;;^UTILITY(U,$J,358.3,217,1,5,0)
 ;;=5^Alc Dep, Continuous
 ;;^UTILITY(U,$J,358.3,217,2)
 ;;=^268188
 ;;^UTILITY(U,$J,358.3,218,0)
 ;;=303.92^^2^18^4
 ;;^UTILITY(U,$J,358.3,218,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,218,1,2,0)
 ;;=2^303.92
 ;;^UTILITY(U,$J,358.3,218,1,5,0)
 ;;=5^Alc Dep, Episodic
 ;;^UTILITY(U,$J,358.3,218,2)
 ;;=^268189
 ;;^UTILITY(U,$J,358.3,219,0)
 ;;=304.01^^2^18^73
 ;;^UTILITY(U,$J,358.3,219,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,219,1,2,0)
 ;;=2^304.01
 ;;^UTILITY(U,$J,358.3,219,1,5,0)
 ;;=5^Opioid Depend, Continuous
 ;;^UTILITY(U,$J,358.3,219,2)
 ;;=^268191
 ;;^UTILITY(U,$J,358.3,220,0)
 ;;=304.02^^2^18^75
 ;;^UTILITY(U,$J,358.3,220,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,220,1,2,0)
 ;;=2^304.02
 ;;^UTILITY(U,$J,358.3,220,1,5,0)
 ;;=5^Opioid Dependence, Episodic
 ;;^UTILITY(U,$J,358.3,220,2)
 ;;=^268192
