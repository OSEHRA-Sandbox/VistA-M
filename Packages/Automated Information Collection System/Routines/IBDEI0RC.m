IBDEI0RC ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13490,0)
 ;;=90636^^89^838^5^^^^1
 ;;^UTILITY(U,$J,358.3,13490,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13490,1,2,0)
 ;;=2^90636
 ;;^UTILITY(U,$J,358.3,13490,1,3,0)
 ;;=3^Hepatitis A&B Vaccine
 ;;^UTILITY(U,$J,358.3,13491,0)
 ;;=90707^^89^838^7^^^^1
 ;;^UTILITY(U,$J,358.3,13491,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13491,1,2,0)
 ;;=2^90707
 ;;^UTILITY(U,$J,358.3,13491,1,3,0)
 ;;=3^MMR Vaccine
 ;;^UTILITY(U,$J,358.3,13492,0)
 ;;=90658^^89^838^6^^^^1
 ;;^UTILITY(U,$J,358.3,13492,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13492,1,2,0)
 ;;=2^90658
 ;;^UTILITY(U,$J,358.3,13492,1,3,0)
 ;;=3^Influenza Vaccine
 ;;^UTILITY(U,$J,358.3,13493,0)
 ;;=90732^^89^838^8^^^^1
 ;;^UTILITY(U,$J,358.3,13493,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13493,1,2,0)
 ;;=2^90732
 ;;^UTILITY(U,$J,358.3,13493,1,3,0)
 ;;=3^Pneumococcal Vaccine
 ;;^UTILITY(U,$J,358.3,13494,0)
 ;;=90714^^89^838^10^^^^1
 ;;^UTILITY(U,$J,358.3,13494,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13494,1,2,0)
 ;;=2^90714
 ;;^UTILITY(U,$J,358.3,13494,1,3,0)
 ;;=3^TD Vaccine
 ;;^UTILITY(U,$J,358.3,13495,0)
 ;;=95120^^89^839^2^^^^1
 ;;^UTILITY(U,$J,358.3,13495,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13495,1,2,0)
 ;;=2^95120
 ;;^UTILITY(U,$J,358.3,13495,1,3,0)
 ;;=3^Allergy Injection
 ;;^UTILITY(U,$J,358.3,13496,0)
 ;;=J3301^^89^839^12^^^^1
 ;;^UTILITY(U,$J,358.3,13496,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13496,1,2,0)
 ;;=2^J3301
 ;;^UTILITY(U,$J,358.3,13496,1,3,0)
 ;;=3^Kenalog, per 10 mg
 ;;^UTILITY(U,$J,358.3,13497,0)
 ;;=J9217^^89^839^14^^^^1
 ;;^UTILITY(U,$J,358.3,13497,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13497,1,2,0)
 ;;=2^J9217
 ;;^UTILITY(U,$J,358.3,13497,1,3,0)
 ;;=3^Lupron, per 7.5 mg
 ;;^UTILITY(U,$J,358.3,13498,0)
 ;;=J3120^^89^839^19^^^^1
 ;;^UTILITY(U,$J,358.3,13498,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13498,1,2,0)
 ;;=2^J3120
 ;;^UTILITY(U,$J,358.3,13498,1,3,0)
 ;;=3^Testosterone Enanthate 100mg
 ;;^UTILITY(U,$J,358.3,13499,0)
 ;;=J1885^^89^839^23^^^^1
 ;;^UTILITY(U,$J,358.3,13499,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13499,1,2,0)
 ;;=2^J1885
 ;;^UTILITY(U,$J,358.3,13499,1,3,0)
 ;;=3^Toradol, per 15mg
 ;;^UTILITY(U,$J,358.3,13500,0)
 ;;=J3420^^89^839^24^^^^1
 ;;^UTILITY(U,$J,358.3,13500,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13500,1,2,0)
 ;;=2^J3420
 ;;^UTILITY(U,$J,358.3,13500,1,3,0)
 ;;=3^Vitamin B12, per 1000mcg
 ;;^UTILITY(U,$J,358.3,13501,0)
 ;;=J2270^^89^839^16^^^^1
 ;;^UTILITY(U,$J,358.3,13501,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13501,1,2,0)
 ;;=2^J2270
 ;;^UTILITY(U,$J,358.3,13501,1,3,0)
 ;;=3^Morphine, per 10mg
 ;;^UTILITY(U,$J,358.3,13502,0)
 ;;=J2060^^89^839^3^^^^1
 ;;^UTILITY(U,$J,358.3,13502,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13502,1,2,0)
 ;;=2^J2060
 ;;^UTILITY(U,$J,358.3,13502,1,3,0)
 ;;=3^Ativan/Lorazepam, 2 mg
 ;;^UTILITY(U,$J,358.3,13503,0)
 ;;=J1940^^89^839^13^^^^1
 ;;^UTILITY(U,$J,358.3,13503,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13503,1,2,0)
 ;;=2^J1940
 ;;^UTILITY(U,$J,358.3,13503,1,3,0)
 ;;=3^Lasix, per 20 mg
 ;;^UTILITY(U,$J,358.3,13504,0)
 ;;=J0150^^89^839^1^^^^1
 ;;^UTILITY(U,$J,358.3,13504,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13504,1,2,0)
 ;;=2^J0150
 ;;^UTILITY(U,$J,358.3,13504,1,3,0)
 ;;=3^Adenosine, 6mg
 ;;^UTILITY(U,$J,358.3,13505,0)
 ;;=J0515^^89^839^5^^^^1
 ;;^UTILITY(U,$J,358.3,13505,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,13505,1,2,0)
 ;;=2^J0515
 ;;^UTILITY(U,$J,358.3,13505,1,3,0)
 ;;=3^Benzotropine, per 1mg
 ;;^UTILITY(U,$J,358.3,13506,0)
 ;;=J1630^^89^839^7^^^^1
 ;;^UTILITY(U,$J,358.3,13506,1,0)
 ;;=^358.31IA^3^2
