IBDEI0FW ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,7694,1,5,0)
 ;;=5^Hyperlipidemia,Mixed
 ;;^UTILITY(U,$J,358.3,7694,2)
 ;;=^78424
 ;;^UTILITY(U,$J,358.3,7695,0)
 ;;=401.9^^55^587^36
 ;;^UTILITY(U,$J,358.3,7695,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7695,1,4,0)
 ;;=4^401.9
 ;;^UTILITY(U,$J,358.3,7695,1,5,0)
 ;;=5^Hypertension NOS
 ;;^UTILITY(U,$J,358.3,7695,2)
 ;;=^186630
 ;;^UTILITY(U,$J,358.3,7696,0)
 ;;=244.9^^55^587^37
 ;;^UTILITY(U,$J,358.3,7696,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7696,1,4,0)
 ;;=4^244.9
 ;;^UTILITY(U,$J,358.3,7696,1,5,0)
 ;;=5^Hypothyroidism NOS
 ;;^UTILITY(U,$J,358.3,7696,2)
 ;;=^123752
 ;;^UTILITY(U,$J,358.3,7697,0)
 ;;=790.21^^55^587^38
 ;;^UTILITY(U,$J,358.3,7697,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7697,1,4,0)
 ;;=4^790.21
 ;;^UTILITY(U,$J,358.3,7697,1,5,0)
 ;;=5^Impaired Fasting Glucose
 ;;^UTILITY(U,$J,358.3,7697,2)
 ;;=^329895
 ;;^UTILITY(U,$J,358.3,7698,0)
 ;;=790.5^^55^587^40
 ;;^UTILITY(U,$J,358.3,7698,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7698,1,4,0)
 ;;=4^790.5
 ;;^UTILITY(U,$J,358.3,7698,1,5,0)
 ;;=5^Liver Enzymes,Elevated
 ;;^UTILITY(U,$J,358.3,7698,2)
 ;;=^273402
 ;;^UTILITY(U,$J,358.3,7699,0)
 ;;=724.2^^55^587^41
 ;;^UTILITY(U,$J,358.3,7699,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7699,1,4,0)
 ;;=4^724.2
 ;;^UTILITY(U,$J,358.3,7699,1,5,0)
 ;;=5^Low Back Pain,Chronic
 ;;^UTILITY(U,$J,358.3,7699,2)
 ;;=^71885
 ;;^UTILITY(U,$J,358.3,7700,0)
 ;;=412.^^55^587^45
 ;;^UTILITY(U,$J,358.3,7700,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7700,1,4,0)
 ;;=4^412.
 ;;^UTILITY(U,$J,358.3,7700,1,5,0)
 ;;=5^Myocardial Infarction-Old
 ;;^UTILITY(U,$J,358.3,7700,2)
 ;;=^259884
 ;;^UTILITY(U,$J,358.3,7701,0)
 ;;=250.60^^55^587^47
 ;;^UTILITY(U,$J,358.3,7701,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7701,1,4,0)
 ;;=4^250.60
 ;;^UTILITY(U,$J,358.3,7701,1,5,0)
 ;;=5^Neuropathy,Diabetic
 ;;^UTILITY(U,$J,358.3,7701,2)
 ;;=^331803^357.2
 ;;^UTILITY(U,$J,358.3,7702,0)
 ;;=V15.81^^55^587^48
 ;;^UTILITY(U,$J,358.3,7702,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7702,1,4,0)
 ;;=4^V15.81
 ;;^UTILITY(U,$J,358.3,7702,1,5,0)
 ;;=5^Noncompliance
 ;;^UTILITY(U,$J,358.3,7702,2)
 ;;=^295290
 ;;^UTILITY(U,$J,358.3,7703,0)
 ;;=278.00^^55^587^49
 ;;^UTILITY(U,$J,358.3,7703,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7703,1,4,0)
 ;;=4^278.00
 ;;^UTILITY(U,$J,358.3,7703,1,5,0)
 ;;=5^Obesity
 ;;^UTILITY(U,$J,358.3,7703,2)
 ;;=^84823
 ;;^UTILITY(U,$J,358.3,7704,0)
 ;;=278.02^^55^587^51
 ;;^UTILITY(U,$J,358.3,7704,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7704,1,4,0)
 ;;=4^278.02
 ;;^UTILITY(U,$J,358.3,7704,1,5,0)
 ;;=5^Overweight
 ;;^UTILITY(U,$J,358.3,7704,2)
 ;;=^332745
 ;;^UTILITY(U,$J,358.3,7705,0)
 ;;=715.90^^55^587^50
 ;;^UTILITY(U,$J,358.3,7705,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7705,1,4,0)
 ;;=4^715.90
 ;;^UTILITY(U,$J,358.3,7705,1,5,0)
 ;;=5^Osteoarthritis,Unspec
 ;;^UTILITY(U,$J,358.3,7705,2)
 ;;=^272161
 ;;^UTILITY(U,$J,358.3,7706,0)
 ;;=728.71^^55^587^54
 ;;^UTILITY(U,$J,358.3,7706,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7706,1,4,0)
 ;;=4^728.71
 ;;^UTILITY(U,$J,358.3,7706,1,5,0)
 ;;=5^Plantar Fascitis
 ;;^UTILITY(U,$J,358.3,7706,2)
 ;;=^272598
 ;;^UTILITY(U,$J,358.3,7707,0)
 ;;=486.^^55^587^55
 ;;^UTILITY(U,$J,358.3,7707,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7707,1,4,0)
 ;;=4^486.
 ;;^UTILITY(U,$J,358.3,7707,1,5,0)
 ;;=5^Pneumonia
 ;;^UTILITY(U,$J,358.3,7707,2)
 ;;=^95632
 ;;^UTILITY(U,$J,358.3,7708,0)
 ;;=185.^^55^587^56
 ;;^UTILITY(U,$J,358.3,7708,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,7708,1,4,0)
 ;;=4^185.
 ;;^UTILITY(U,$J,358.3,7708,1,5,0)
 ;;=5^Prostate Cancer
