IBDEI0BT ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,5075,1,2,0)
 ;;=2^Natalizumab Inj 1mg
 ;;^UTILITY(U,$J,358.3,5075,1,4,0)
 ;;=4^J2323
 ;;^UTILITY(U,$J,358.3,5076,0)
 ;;=J9310^^39^348^19^^^^1
 ;;^UTILITY(U,$J,358.3,5076,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5076,1,2,0)
 ;;=2^Rituximab Inj 100mg
 ;;^UTILITY(U,$J,358.3,5076,1,4,0)
 ;;=4^J9310
 ;;^UTILITY(U,$J,358.3,5077,0)
 ;;=J1094^^39^348^5^^^^1
 ;;^UTILITY(U,$J,358.3,5077,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5077,1,2,0)
 ;;=2^Dexamethasone Acetate 1mg
 ;;^UTILITY(U,$J,358.3,5077,1,4,0)
 ;;=4^J1094
 ;;^UTILITY(U,$J,358.3,5078,0)
 ;;=J0256^^39^348^1^^^^1
 ;;^UTILITY(U,$J,358.3,5078,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5078,1,2,0)
 ;;=2^Alpha 1 Proteinase Inhib 10mg
 ;;^UTILITY(U,$J,358.3,5078,1,4,0)
 ;;=4^J0256
 ;;^UTILITY(U,$J,358.3,5079,0)
 ;;=J3370^^39^348^20^^^^1
 ;;^UTILITY(U,$J,358.3,5079,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5079,1,2,0)
 ;;=2^Vancomycin HCL Inj 500mg
 ;;^UTILITY(U,$J,358.3,5079,1,4,0)
 ;;=4^J3370
 ;;^UTILITY(U,$J,358.3,5080,0)
 ;;=J3475^^39^348^15^^^^1
 ;;^UTILITY(U,$J,358.3,5080,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5080,1,2,0)
 ;;=2^Magnesium Sulfate Inj 500mg
 ;;^UTILITY(U,$J,358.3,5080,1,4,0)
 ;;=4^J3475
 ;;^UTILITY(U,$J,358.3,5081,0)
 ;;=J3480^^39^348^18^^^^1
 ;;^UTILITY(U,$J,358.3,5081,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5081,1,2,0)
 ;;=2^Potassium Chloride Inj 2mEq
 ;;^UTILITY(U,$J,358.3,5081,1,4,0)
 ;;=4^J3480
 ;;^UTILITY(U,$J,358.3,5082,0)
 ;;=J9070^^39^348^2^^^^1
 ;;^UTILITY(U,$J,358.3,5082,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5082,1,2,0)
 ;;=2^Cyclophosphamide Inj 100mg
 ;;^UTILITY(U,$J,358.3,5082,1,4,0)
 ;;=4^J9070
 ;;^UTILITY(U,$J,358.3,5083,0)
 ;;=J8530^^39^348^3^^^^1
 ;;^UTILITY(U,$J,358.3,5083,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5083,1,2,0)
 ;;=2^Cyclophosphamide Oral 25mg
 ;;^UTILITY(U,$J,358.3,5083,1,4,0)
 ;;=4^J8530
 ;;^UTILITY(U,$J,358.3,5084,0)
 ;;=J1020^^39^348^16^^^^1
 ;;^UTILITY(U,$J,358.3,5084,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5084,1,2,0)
 ;;=2^Methylprednisolone Inj 20mg
 ;;^UTILITY(U,$J,358.3,5084,1,4,0)
 ;;=4^J1020
 ;;^UTILITY(U,$J,358.3,5085,0)
 ;;=J1745^^39^348^12^^^^1
 ;;^UTILITY(U,$J,358.3,5085,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5085,1,2,0)
 ;;=2^Infliximab Inj 10mg
 ;;^UTILITY(U,$J,358.3,5085,1,4,0)
 ;;=4^J1745
 ;;^UTILITY(U,$J,358.3,5086,0)
 ;;=J1200^^39^348^6^^^^1
 ;;^UTILITY(U,$J,358.3,5086,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5086,1,2,0)
 ;;=2^Diphenhydramine HCL Inj 50mg
 ;;^UTILITY(U,$J,358.3,5086,1,4,0)
 ;;=4^J1200
 ;;^UTILITY(U,$J,358.3,5087,0)
 ;;=J1569^^39^348^9^^^^1
 ;;^UTILITY(U,$J,358.3,5087,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5087,1,2,0)
 ;;=2^Gammagard Liquid Inj 500mg
 ;;^UTILITY(U,$J,358.3,5087,1,4,0)
 ;;=4^J1569
 ;;^UTILITY(U,$J,358.3,5088,0)
 ;;=J2916^^39^348^7^^^^1
 ;;^UTILITY(U,$J,358.3,5088,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5088,1,2,0)
 ;;=2^Ferric Gluconate Complex Inj 12.5mg
 ;;^UTILITY(U,$J,358.3,5088,1,4,0)
 ;;=4^J2916
 ;;^UTILITY(U,$J,358.3,5089,0)
 ;;=J1566^^39^348^11^^^^1
 ;;^UTILITY(U,$J,358.3,5089,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5089,1,2,0)
 ;;=2^Immune Globulin Powder 500mg
 ;;^UTILITY(U,$J,358.3,5089,1,4,0)
 ;;=4^J1566
 ;;^UTILITY(U,$J,358.3,5090,0)
 ;;=J0897^^39^348^4^^^^1
 ;;^UTILITY(U,$J,358.3,5090,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5090,1,2,0)
 ;;=2^Denosumab Inj 1mg
 ;;^UTILITY(U,$J,358.3,5090,1,4,0)
 ;;=4^J0897
 ;;^UTILITY(U,$J,358.3,5091,0)
 ;;=J1572^^39^348^8^^^^1
 ;;^UTILITY(U,$J,358.3,5091,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,5091,1,2,0)
 ;;=2^Flebogamma IV 500mg
