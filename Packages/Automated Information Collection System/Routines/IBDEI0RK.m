IBDEI0RK ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13613,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13613,1,4,0)
 ;;=4^427.9
 ;;^UTILITY(U,$J,358.3,13613,1,5,0)
 ;;=5^Cardiac Dysrythmia
 ;;^UTILITY(U,$J,358.3,13613,2)
 ;;=^10166
 ;;^UTILITY(U,$J,358.3,13614,0)
 ;;=427.0^^90^848^70
 ;;^UTILITY(U,$J,358.3,13614,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13614,1,4,0)
 ;;=4^427.0
 ;;^UTILITY(U,$J,358.3,13614,1,5,0)
 ;;=5^Paroxysmal Supravent Tachycardia
 ;;^UTILITY(U,$J,358.3,13614,2)
 ;;=^90479
 ;;^UTILITY(U,$J,358.3,13615,0)
 ;;=427.81^^90^848^79
 ;;^UTILITY(U,$J,358.3,13615,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13615,1,4,0)
 ;;=4^427.81
 ;;^UTILITY(U,$J,358.3,13615,1,5,0)
 ;;=5^Sick Sinus Syndrome
 ;;^UTILITY(U,$J,358.3,13615,2)
 ;;=^110852
 ;;^UTILITY(U,$J,358.3,13616,0)
 ;;=785.0^^90^848^81
 ;;^UTILITY(U,$J,358.3,13616,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13616,1,4,0)
 ;;=4^785.0
 ;;^UTILITY(U,$J,358.3,13616,1,5,0)
 ;;=5^Tachycardia
 ;;^UTILITY(U,$J,358.3,13616,2)
 ;;=Tachycardia^117041
 ;;^UTILITY(U,$J,358.3,13617,0)
 ;;=785.1^^90^848^69
 ;;^UTILITY(U,$J,358.3,13617,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13617,1,4,0)
 ;;=4^785.1
 ;;^UTILITY(U,$J,358.3,13617,1,5,0)
 ;;=5^Palpitations
 ;;^UTILITY(U,$J,358.3,13617,2)
 ;;=Palpitations^89281
 ;;^UTILITY(U,$J,358.3,13618,0)
 ;;=424.1^^90^848^9
 ;;^UTILITY(U,$J,358.3,13618,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13618,1,4,0)
 ;;=4^424.1
 ;;^UTILITY(U,$J,358.3,13618,1,5,0)
 ;;=5^Aortic Stenosis
 ;;^UTILITY(U,$J,358.3,13618,2)
 ;;=Aortic Stenosis^9330
 ;;^UTILITY(U,$J,358.3,13619,0)
 ;;=424.0^^90^848^62
 ;;^UTILITY(U,$J,358.3,13619,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13619,1,4,0)
 ;;=4^424.0
 ;;^UTILITY(U,$J,358.3,13619,1,5,0)
 ;;=5^Mitral Valve Prolapse
 ;;^UTILITY(U,$J,358.3,13619,2)
 ;;=^78367
 ;;^UTILITY(U,$J,358.3,13620,0)
 ;;=394.0^^90^848^61
 ;;^UTILITY(U,$J,358.3,13620,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13620,1,4,0)
 ;;=4^394.0
 ;;^UTILITY(U,$J,358.3,13620,1,5,0)
 ;;=5^Mitral Stenosis
 ;;^UTILITY(U,$J,358.3,13620,2)
 ;;=Mitral Stenosis^78404
 ;;^UTILITY(U,$J,358.3,13621,0)
 ;;=394.9^^90^848^38
 ;;^UTILITY(U,$J,358.3,13621,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13621,1,4,0)
 ;;=4^394.9
 ;;^UTILITY(U,$J,358.3,13621,1,5,0)
 ;;=5^Heart Dis Mitral Valve
 ;;^UTILITY(U,$J,358.3,13621,2)
 ;;=^269571
 ;;^UTILITY(U,$J,358.3,13622,0)
 ;;=397.1^^90^848^39
 ;;^UTILITY(U,$J,358.3,13622,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13622,1,4,0)
 ;;=4^397.1
 ;;^UTILITY(U,$J,358.3,13622,1,5,0)
 ;;=5^Heart Dis Pulmon Valve
 ;;^UTILITY(U,$J,358.3,13622,2)
 ;;=^269587
 ;;^UTILITY(U,$J,358.3,13623,0)
 ;;=397.0^^90^848^40
 ;;^UTILITY(U,$J,358.3,13623,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13623,1,4,0)
 ;;=4^397.0
 ;;^UTILITY(U,$J,358.3,13623,1,5,0)
 ;;=5^Heart Dis Tricuspid Valve
 ;;^UTILITY(U,$J,358.3,13623,2)
 ;;=^35528
 ;;^UTILITY(U,$J,358.3,13624,0)
 ;;=424.90^^90^848^83
 ;;^UTILITY(U,$J,358.3,13624,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13624,1,4,0)
 ;;=4^424.90
 ;;^UTILITY(U,$J,358.3,13624,1,5,0)
 ;;=5^Valvular Heart Disease
 ;;^UTILITY(U,$J,358.3,13624,2)
 ;;=^40327
 ;;^UTILITY(U,$J,358.3,13625,0)
 ;;=V43.3^^90^848^77
 ;;^UTILITY(U,$J,358.3,13625,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13625,1,4,0)
 ;;=4^V43.3
 ;;^UTILITY(U,$J,358.3,13625,1,5,0)
 ;;=5^S/P Heart Valve Replacement
 ;;^UTILITY(U,$J,358.3,13625,2)
 ;;=^295440
 ;;^UTILITY(U,$J,358.3,13626,0)
 ;;=401.1^^90^848^56
 ;;^UTILITY(U,$J,358.3,13626,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13626,1,4,0)
 ;;=4^401.1
 ;;^UTILITY(U,$J,358.3,13626,1,5,0)
 ;;=5^Hypertension, Benign
