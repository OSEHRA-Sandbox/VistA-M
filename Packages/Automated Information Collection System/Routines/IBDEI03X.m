IBDEI03X ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,3641,1,2,0)
 ;;=2^Intro Needle or Intracath,Vein
 ;;^UTILITY(U,$J,358.3,3641,1,4,0)
 ;;=4^36000
 ;;^UTILITY(U,$J,358.3,3642,0)
 ;;=99195^^29^282^9^^^^1
 ;;^UTILITY(U,$J,358.3,3642,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3642,1,2,0)
 ;;=2^Phlebotomy
 ;;^UTILITY(U,$J,358.3,3642,1,4,0)
 ;;=4^99195
 ;;^UTILITY(U,$J,358.3,3643,0)
 ;;=4066F^^29^282^3^^^^1
 ;;^UTILITY(U,$J,358.3,3643,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3643,1,2,0)
 ;;=2^ECT Provided
 ;;^UTILITY(U,$J,358.3,3643,1,4,0)
 ;;=4^4066F
 ;;^UTILITY(U,$J,358.3,3644,0)
 ;;=P9047^^29^283^3^^^^1
 ;;^UTILITY(U,$J,358.3,3644,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3644,1,2,0)
 ;;=2^Inf Albumin(Human),25%,50ml
 ;;^UTILITY(U,$J,358.3,3644,1,4,0)
 ;;=4^P9047
 ;;^UTILITY(U,$J,358.3,3645,0)
 ;;=P9010^^29^283^6^^^^1
 ;;^UTILITY(U,$J,358.3,3645,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3645,1,2,0)
 ;;=2^Whole Blood for Transfusion,per unit
 ;;^UTILITY(U,$J,358.3,3645,1,4,0)
 ;;=4^P9010
 ;;^UTILITY(U,$J,358.3,3646,0)
 ;;=P9017^^29^283^2^^^^1
 ;;^UTILITY(U,$J,358.3,3646,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3646,1,2,0)
 ;;=2^Frh Frz Plasma 1 Donor w/in 8hr,per unit
 ;;^UTILITY(U,$J,358.3,3646,1,4,0)
 ;;=4^P9017
 ;;^UTILITY(U,$J,358.3,3647,0)
 ;;=P9020^^29^283^4^^^^1
 ;;^UTILITY(U,$J,358.3,3647,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3647,1,2,0)
 ;;=2^Platelet,Rich Plasma,per unit
 ;;^UTILITY(U,$J,358.3,3647,1,4,0)
 ;;=4^P9020
 ;;^UTILITY(U,$J,358.3,3648,0)
 ;;=P9021^^29^283^5^^^^1
 ;;^UTILITY(U,$J,358.3,3648,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3648,1,2,0)
 ;;=2^Red Blood Cells,per unit
 ;;^UTILITY(U,$J,358.3,3648,1,4,0)
 ;;=4^P9021
 ;;^UTILITY(U,$J,358.3,3649,0)
 ;;=36430^^29^283^1^^^^1
 ;;^UTILITY(U,$J,358.3,3649,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3649,1,2,0)
 ;;=2^Blood Transfusion Service
 ;;^UTILITY(U,$J,358.3,3649,1,4,0)
 ;;=4^36430
 ;;^UTILITY(U,$J,358.3,3650,0)
 ;;=96360^^29^284^1^^^^1
 ;;^UTILITY(U,$J,358.3,3650,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3650,1,2,0)
 ;;=2^Hydration IV Inf,Init,31-60 mins
 ;;^UTILITY(U,$J,358.3,3650,1,4,0)
 ;;=4^96360
 ;;^UTILITY(U,$J,358.3,3651,0)
 ;;=96361^^29^284^2^^^^1
 ;;^UTILITY(U,$J,358.3,3651,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3651,1,2,0)
 ;;=2^Hydration IV Inf,Ea Addl Hr
 ;;^UTILITY(U,$J,358.3,3651,1,4,0)
 ;;=4^96361
 ;;^UTILITY(U,$J,358.3,3652,0)
 ;;=96365^^29^284^3^^^^1
 ;;^UTILITY(U,$J,358.3,3652,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3652,1,2,0)
 ;;=2^IV Inf Ther/Proph/Diag Init Hr
 ;;^UTILITY(U,$J,358.3,3652,1,4,0)
 ;;=4^96365
 ;;^UTILITY(U,$J,358.3,3653,0)
 ;;=96366^^29^284^4^^^^1
 ;;^UTILITY(U,$J,358.3,3653,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3653,1,2,0)
 ;;=2^IV Inf Ther/Proph/Diag Ea Addl Hr
 ;;^UTILITY(U,$J,358.3,3653,1,4,0)
 ;;=4^96366
 ;;^UTILITY(U,$J,358.3,3654,0)
 ;;=J1750^^29^285^13^^^^1
 ;;^UTILITY(U,$J,358.3,3654,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3654,1,2,0)
 ;;=2^Iron Dextran Inj 50mg
 ;;^UTILITY(U,$J,358.3,3654,1,4,0)
 ;;=4^J1750
 ;;^UTILITY(U,$J,358.3,3655,0)
 ;;=J1756^^29^285^14^^^^1
 ;;^UTILITY(U,$J,358.3,3655,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3655,1,2,0)
 ;;=2^Iron Sucrose Inj 1mg/ml
 ;;^UTILITY(U,$J,358.3,3655,1,4,0)
 ;;=4^J1756
 ;;^UTILITY(U,$J,358.3,3656,0)
 ;;=J2323^^29^285^17^^^^1
 ;;^UTILITY(U,$J,358.3,3656,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3656,1,2,0)
 ;;=2^Natalizumab Inj 1mg
 ;;^UTILITY(U,$J,358.3,3656,1,4,0)
 ;;=4^J2323
 ;;^UTILITY(U,$J,358.3,3657,0)
 ;;=J9310^^29^285^19^^^^1
 ;;^UTILITY(U,$J,358.3,3657,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3657,1,2,0)
 ;;=2^Rituximab Inj 100mg
 ;;^UTILITY(U,$J,358.3,3657,1,4,0)
 ;;=4^J9310
 ;;^UTILITY(U,$J,358.3,3658,0)
 ;;=J1094^^29^285^5^^^^1
 ;;^UTILITY(U,$J,358.3,3658,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3658,1,2,0)
 ;;=2^Dexamethasone Acetate 1mg
 ;;^UTILITY(U,$J,358.3,3658,1,4,0)
 ;;=4^J1094
 ;;^UTILITY(U,$J,358.3,3659,0)
 ;;=J0256^^29^285^1^^^^1
 ;;^UTILITY(U,$J,358.3,3659,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3659,1,2,0)
 ;;=2^Alpha 1 Proteinase Inhib 10mg
 ;;^UTILITY(U,$J,358.3,3659,1,4,0)
 ;;=4^J0256
 ;;^UTILITY(U,$J,358.3,3660,0)
 ;;=J3370^^29^285^20^^^^1
 ;;^UTILITY(U,$J,358.3,3660,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3660,1,2,0)
 ;;=2^Vancomycin HCL Inj 500mg
 ;;^UTILITY(U,$J,358.3,3660,1,4,0)
 ;;=4^J3370
 ;;^UTILITY(U,$J,358.3,3661,0)
 ;;=J3475^^29^285^15^^^^1
 ;;^UTILITY(U,$J,358.3,3661,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3661,1,2,0)
 ;;=2^Magnesium Sulfate Inj 500mg
 ;;^UTILITY(U,$J,358.3,3661,1,4,0)
 ;;=4^J3475
 ;;^UTILITY(U,$J,358.3,3662,0)
 ;;=J3480^^29^285^18^^^^1
 ;;^UTILITY(U,$J,358.3,3662,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3662,1,2,0)
 ;;=2^Potassium Chloride Inj 2mEq
 ;;^UTILITY(U,$J,358.3,3662,1,4,0)
 ;;=4^J3480
 ;;^UTILITY(U,$J,358.3,3663,0)
 ;;=J9070^^29^285^2^^^^1
 ;;^UTILITY(U,$J,358.3,3663,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3663,1,2,0)
 ;;=2^Cyclophosphamide Inj 100mg
 ;;^UTILITY(U,$J,358.3,3663,1,4,0)
 ;;=4^J9070
 ;;^UTILITY(U,$J,358.3,3664,0)
 ;;=J8530^^29^285^3^^^^1
 ;;^UTILITY(U,$J,358.3,3664,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3664,1,2,0)
 ;;=2^Cyclophosphamide Oral 25mg
 ;;^UTILITY(U,$J,358.3,3664,1,4,0)
 ;;=4^J8530
 ;;^UTILITY(U,$J,358.3,3665,0)
 ;;=J1020^^29^285^16^^^^1
 ;;^UTILITY(U,$J,358.3,3665,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3665,1,2,0)
 ;;=2^Methylprednisolone Inj 20mg
 ;;^UTILITY(U,$J,358.3,3665,1,4,0)
 ;;=4^J1020
 ;;^UTILITY(U,$J,358.3,3666,0)
 ;;=J1745^^29^285^12^^^^1
 ;;^UTILITY(U,$J,358.3,3666,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3666,1,2,0)
 ;;=2^Infliximab Inj 10mg
 ;;^UTILITY(U,$J,358.3,3666,1,4,0)
 ;;=4^J1745
 ;;^UTILITY(U,$J,358.3,3667,0)
 ;;=J1200^^29^285^6^^^^1
 ;;^UTILITY(U,$J,358.3,3667,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3667,1,2,0)
 ;;=2^Diphenhydramine HCL Inj 50mg
 ;;^UTILITY(U,$J,358.3,3667,1,4,0)
 ;;=4^J1200
 ;;^UTILITY(U,$J,358.3,3668,0)
 ;;=J1569^^29^285^9^^^^1
 ;;^UTILITY(U,$J,358.3,3668,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3668,1,2,0)
 ;;=2^Gammagard Liquid Inj 500mg
 ;;^UTILITY(U,$J,358.3,3668,1,4,0)
 ;;=4^J1569
 ;;^UTILITY(U,$J,358.3,3669,0)
 ;;=J2916^^29^285^7^^^^1
 ;;^UTILITY(U,$J,358.3,3669,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3669,1,2,0)
 ;;=2^Ferric Gluconate Complex Inj 12.5mg
 ;;^UTILITY(U,$J,358.3,3669,1,4,0)
 ;;=4^J2916
 ;;^UTILITY(U,$J,358.3,3670,0)
 ;;=J1566^^29^285^11^^^^1
 ;;^UTILITY(U,$J,358.3,3670,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3670,1,2,0)
 ;;=2^Immune Globulin Powder 500mg
 ;;^UTILITY(U,$J,358.3,3670,1,4,0)
 ;;=4^J1566
 ;;^UTILITY(U,$J,358.3,3671,0)
 ;;=J0897^^29^285^4^^^^1
 ;;^UTILITY(U,$J,358.3,3671,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3671,1,2,0)
 ;;=2^Denosumab Inj 1mg
 ;;^UTILITY(U,$J,358.3,3671,1,4,0)
 ;;=4^J0897
 ;;^UTILITY(U,$J,358.3,3672,0)
 ;;=J1572^^29^285^8^^^^1
 ;;^UTILITY(U,$J,358.3,3672,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3672,1,2,0)
 ;;=2^Flebogamma IV 500mg
 ;;^UTILITY(U,$J,358.3,3672,1,4,0)
 ;;=4^J1572
 ;;^UTILITY(U,$J,358.3,3673,0)
 ;;=J1561^^29^285^10^^^^1
 ;;^UTILITY(U,$J,358.3,3673,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3673,1,2,0)
 ;;=2^Gamunex,Gamunex-C,Gammaked Inj 500mg
 ;;^UTILITY(U,$J,358.3,3673,1,4,0)
 ;;=4^J1561
 ;;^UTILITY(U,$J,358.3,3674,0)
 ;;=96372^^29^286^1^^^^1
 ;;^UTILITY(U,$J,358.3,3674,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3674,1,2,0)
 ;;=2^Inj Admin SC/IM
 ;;^UTILITY(U,$J,358.3,3674,1,4,0)
 ;;=4^96372
 ;;^UTILITY(U,$J,358.3,3675,0)
 ;;=96374^^29^286^2^^^^1
 ;;^UTILITY(U,$J,358.3,3675,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3675,1,2,0)
 ;;=2^IV Push Single/Init Subst/Drug
 ;;^UTILITY(U,$J,358.3,3675,1,4,0)
 ;;=4^96374
 ;;^UTILITY(U,$J,358.3,3676,0)
 ;;=96375^^29^286^3^^^^1
