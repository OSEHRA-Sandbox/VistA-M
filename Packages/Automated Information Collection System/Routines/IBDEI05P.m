IBDEI05P ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,2441,0)
 ;;=459.81^^27^234^23
 ;;^UTILITY(U,$J,358.3,2441,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2441,1,4,0)
 ;;=4^459.81
 ;;^UTILITY(U,$J,358.3,2441,1,5,0)
 ;;=5^Venous Insufficiency
 ;;^UTILITY(U,$J,358.3,2441,2)
 ;;=^125826
 ;;^UTILITY(U,$J,358.3,2442,0)
 ;;=V45.01^^27^234^20
 ;;^UTILITY(U,$J,358.3,2442,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2442,1,4,0)
 ;;=4^V45.01
 ;;^UTILITY(U,$J,358.3,2442,1,5,0)
 ;;=5^S/P Pacer Placement
 ;;^UTILITY(U,$J,358.3,2442,2)
 ;;=^303419
 ;;^UTILITY(U,$J,358.3,2443,0)
 ;;=427.81^^27^234^21
 ;;^UTILITY(U,$J,358.3,2443,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2443,1,4,0)
 ;;=4^427.81
 ;;^UTILITY(U,$J,358.3,2443,1,5,0)
 ;;=5^Sick Sinus Syndrome
 ;;^UTILITY(U,$J,358.3,2443,2)
 ;;=^110852
 ;;^UTILITY(U,$J,358.3,2444,0)
 ;;=424.1^^27^234^3
 ;;^UTILITY(U,$J,358.3,2444,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2444,1,4,0)
 ;;=4^424.1
 ;;^UTILITY(U,$J,358.3,2444,1,5,0)
 ;;=5^Aortic Stenosis
 ;;^UTILITY(U,$J,358.3,2444,2)
 ;;=Aortic Stenosis^9330
 ;;^UTILITY(U,$J,358.3,2445,0)
 ;;=424.0^^27^234^14
 ;;^UTILITY(U,$J,358.3,2445,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2445,1,4,0)
 ;;=4^424.0
 ;;^UTILITY(U,$J,358.3,2445,1,5,0)
 ;;=5^Mitral Valve Prolapse
 ;;^UTILITY(U,$J,358.3,2445,2)
 ;;=^78367
 ;;^UTILITY(U,$J,358.3,2446,0)
 ;;=394.0^^27^234^13
 ;;^UTILITY(U,$J,358.3,2446,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2446,1,4,0)
 ;;=4^394.0
 ;;^UTILITY(U,$J,358.3,2446,1,5,0)
 ;;=5^Mitral Stenosis
 ;;^UTILITY(U,$J,358.3,2446,2)
 ;;=Mitral Stenosis^78404
 ;;^UTILITY(U,$J,358.3,2447,0)
 ;;=V43.3^^27^234^19
 ;;^UTILITY(U,$J,358.3,2447,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2447,1,4,0)
 ;;=4^V43.3
 ;;^UTILITY(U,$J,358.3,2447,1,5,0)
 ;;=5^S/P Heart Valve Replacement
 ;;^UTILITY(U,$J,358.3,2447,2)
 ;;=^295440
 ;;^UTILITY(U,$J,358.3,2448,0)
 ;;=396.0^^27^234^5
 ;;^UTILITY(U,$J,358.3,2448,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2448,1,4,0)
 ;;=4^396.0
 ;;^UTILITY(U,$J,358.3,2448,1,5,0)
 ;;=5^Aortic and Mitral Stenosis
 ;;^UTILITY(U,$J,358.3,2448,2)
 ;;=Aortic and Mitral Stenosis^269580
 ;;^UTILITY(U,$J,358.3,2449,0)
 ;;=396.3^^27^234^4
 ;;^UTILITY(U,$J,358.3,2449,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2449,1,4,0)
 ;;=4^396.3
 ;;^UTILITY(U,$J,358.3,2449,1,5,0)
 ;;=5^Aortic and Mitral Insufficiency
 ;;^UTILITY(U,$J,358.3,2449,2)
 ;;=Aortic and Mitral Insufficiency^269583
 ;;^UTILITY(U,$J,358.3,2450,0)
 ;;=454.9^^27^234^22
 ;;^UTILITY(U,$J,358.3,2450,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2450,1,4,0)
 ;;=4^454.9
 ;;^UTILITY(U,$J,358.3,2450,1,5,0)
 ;;=5^Varicose Veins
 ;;^UTILITY(U,$J,358.3,2450,2)
 ;;=^328758
 ;;^UTILITY(U,$J,358.3,2451,0)
 ;;=414.01^^27^234^6
 ;;^UTILITY(U,$J,358.3,2451,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2451,1,4,0)
 ;;=4^414.01
 ;;^UTILITY(U,$J,358.3,2451,1,5,0)
 ;;=5^CAD Native Artery
 ;;^UTILITY(U,$J,358.3,2451,2)
 ;;=^303281
 ;;^UTILITY(U,$J,358.3,2452,0)
 ;;=414.00^^27^234^7
 ;;^UTILITY(U,$J,358.3,2452,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2452,1,4,0)
 ;;=4^414.00
 ;;^UTILITY(U,$J,358.3,2452,1,5,0)
 ;;=5^CAD,S/P CABG
 ;;^UTILITY(U,$J,358.3,2452,2)
 ;;=^28512
 ;;^UTILITY(U,$J,358.3,2453,0)
 ;;=425.4^^27^234^9
 ;;^UTILITY(U,$J,358.3,2453,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2453,1,4,0)
 ;;=4^425.4
 ;;^UTILITY(U,$J,358.3,2453,1,5,0)
 ;;=5^Cardiomyopathy
 ;;^UTILITY(U,$J,358.3,2453,2)
 ;;=^87808
 ;;^UTILITY(U,$J,358.3,2454,0)
 ;;=786.50^^27^234^11
 ;;^UTILITY(U,$J,358.3,2454,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,2454,1,4,0)
 ;;=4^786.50
 ;;^UTILITY(U,$J,358.3,2454,1,5,0)
 ;;=5^Chest Pain
