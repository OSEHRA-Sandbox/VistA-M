IBDEI15W ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,20805,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20805,1,4,0)
 ;;=4^V15.81
 ;;^UTILITY(U,$J,358.3,20805,1,5,0)
 ;;=5^Noncompliance
 ;;^UTILITY(U,$J,358.3,20805,2)
 ;;=^295290
 ;;^UTILITY(U,$J,358.3,20806,0)
 ;;=278.00^^131^1296^49
 ;;^UTILITY(U,$J,358.3,20806,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20806,1,4,0)
 ;;=4^278.00
 ;;^UTILITY(U,$J,358.3,20806,1,5,0)
 ;;=5^Obesity
 ;;^UTILITY(U,$J,358.3,20806,2)
 ;;=^84823
 ;;^UTILITY(U,$J,358.3,20807,0)
 ;;=278.02^^131^1296^51
 ;;^UTILITY(U,$J,358.3,20807,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20807,1,4,0)
 ;;=4^278.02
 ;;^UTILITY(U,$J,358.3,20807,1,5,0)
 ;;=5^Overweight
 ;;^UTILITY(U,$J,358.3,20807,2)
 ;;=^332745
 ;;^UTILITY(U,$J,358.3,20808,0)
 ;;=715.90^^131^1296^50
 ;;^UTILITY(U,$J,358.3,20808,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20808,1,4,0)
 ;;=4^715.90
 ;;^UTILITY(U,$J,358.3,20808,1,5,0)
 ;;=5^Osteoarthritis,Unspec
 ;;^UTILITY(U,$J,358.3,20808,2)
 ;;=^272161
 ;;^UTILITY(U,$J,358.3,20809,0)
 ;;=728.71^^131^1296^54
 ;;^UTILITY(U,$J,358.3,20809,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20809,1,4,0)
 ;;=4^728.71
 ;;^UTILITY(U,$J,358.3,20809,1,5,0)
 ;;=5^Plantar Fascitis
 ;;^UTILITY(U,$J,358.3,20809,2)
 ;;=^272598
 ;;^UTILITY(U,$J,358.3,20810,0)
 ;;=486.^^131^1296^55
 ;;^UTILITY(U,$J,358.3,20810,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20810,1,4,0)
 ;;=4^486.
 ;;^UTILITY(U,$J,358.3,20810,1,5,0)
 ;;=5^Pneumonia
 ;;^UTILITY(U,$J,358.3,20810,2)
 ;;=^95632
 ;;^UTILITY(U,$J,358.3,20811,0)
 ;;=185.^^131^1296^56
 ;;^UTILITY(U,$J,358.3,20811,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20811,1,4,0)
 ;;=4^185.
 ;;^UTILITY(U,$J,358.3,20811,1,5,0)
 ;;=5^Prostate Cancer
 ;;^UTILITY(U,$J,358.3,20811,2)
 ;;=^99481
 ;;^UTILITY(U,$J,358.3,20812,0)
 ;;=791.0^^131^1296^57
 ;;^UTILITY(U,$J,358.3,20812,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20812,1,4,0)
 ;;=4^791.0
 ;;^UTILITY(U,$J,358.3,20812,1,5,0)
 ;;=5^Proteinuria
 ;;^UTILITY(U,$J,358.3,20812,2)
 ;;=^99873
 ;;^UTILITY(U,$J,358.3,20813,0)
 ;;=790.93^^131^1296^52
 ;;^UTILITY(U,$J,358.3,20813,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20813,1,4,0)
 ;;=4^790.93
 ;;^UTILITY(U,$J,358.3,20813,1,5,0)
 ;;=5^PSA,Elevated
 ;;^UTILITY(U,$J,358.3,20813,2)
 ;;=^334262
 ;;^UTILITY(U,$J,358.3,20814,0)
 ;;=309.81^^131^1296^53
 ;;^UTILITY(U,$J,358.3,20814,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20814,1,4,0)
 ;;=4^309.81
 ;;^UTILITY(U,$J,358.3,20814,1,5,0)
 ;;=5^PTSD
 ;;^UTILITY(U,$J,358.3,20814,2)
 ;;=^114692
 ;;^UTILITY(U,$J,358.3,20815,0)
 ;;=473.8^^131^1296^58
 ;;^UTILITY(U,$J,358.3,20815,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20815,1,4,0)
 ;;=4^473.8
 ;;^UTILITY(U,$J,358.3,20815,1,5,0)
 ;;=5^Sinusitis,Chronic
 ;;^UTILITY(U,$J,358.3,20815,2)
 ;;=^269890
 ;;^UTILITY(U,$J,358.3,20816,0)
 ;;=780.57^^131^1296^60
 ;;^UTILITY(U,$J,358.3,20816,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20816,1,4,0)
 ;;=4^780.57
 ;;^UTILITY(U,$J,358.3,20816,1,5,0)
 ;;=5^Sleep Apnea
 ;;^UTILITY(U,$J,358.3,20816,2)
 ;;=^332928
 ;;^UTILITY(U,$J,358.3,20817,0)
 ;;=722.10^^131^1296^61
 ;;^UTILITY(U,$J,358.3,20817,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20817,1,4,0)
 ;;=4^722.10
 ;;^UTILITY(U,$J,358.3,20817,1,5,0)
 ;;=5^Spine HNP
 ;;^UTILITY(U,$J,358.3,20817,2)
 ;;=^272469
 ;;^UTILITY(U,$J,358.3,20818,0)
 ;;=436.^^131^1296^62
 ;;^UTILITY(U,$J,358.3,20818,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,20818,1,4,0)
 ;;=4^436.
 ;;^UTILITY(U,$J,358.3,20818,1,5,0)
 ;;=5^Stroke
 ;;^UTILITY(U,$J,358.3,20818,2)
 ;;=^2710
 ;;^UTILITY(U,$J,358.3,20819,0)
 ;;=305.90^^131^1296^63
 ;;^UTILITY(U,$J,358.3,20819,1,0)
 ;;=^358.31IA^5^2
