IBDEI0RU ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13746,0)
 ;;=011.90^^90^850^37
 ;;^UTILITY(U,$J,358.3,13746,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13746,1,4,0)
 ;;=4^011.90
 ;;^UTILITY(U,$J,358.3,13746,1,5,0)
 ;;=5^TB, Pulmonary, NOS
 ;;^UTILITY(U,$J,358.3,13746,2)
 ;;=TB, Pulmonary^122756
 ;;^UTILITY(U,$J,358.3,13747,0)
 ;;=786.06^^90^850^38
 ;;^UTILITY(U,$J,358.3,13747,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13747,1,4,0)
 ;;=4^786.06
 ;;^UTILITY(U,$J,358.3,13747,1,5,0)
 ;;=5^Tachypnea
 ;;^UTILITY(U,$J,358.3,13747,2)
 ;;=Tachypnea^321213
 ;;^UTILITY(U,$J,358.3,13748,0)
 ;;=305.1^^90^850^39
 ;;^UTILITY(U,$J,358.3,13748,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13748,1,4,0)
 ;;=4^305.1
 ;;^UTILITY(U,$J,358.3,13748,1,5,0)
 ;;=5^Tobacco Use
 ;;^UTILITY(U,$J,358.3,13748,2)
 ;;=Tobacco Use^119899
 ;;^UTILITY(U,$J,358.3,13749,0)
 ;;=786.07^^90^850^40
 ;;^UTILITY(U,$J,358.3,13749,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13749,1,4,0)
 ;;=4^786.07
 ;;^UTILITY(U,$J,358.3,13749,1,5,0)
 ;;=5^Wheezing
 ;;^UTILITY(U,$J,358.3,13749,2)
 ;;=Wheezing^127848
 ;;^UTILITY(U,$J,358.3,13750,0)
 ;;=519.11^^90^850^4
 ;;^UTILITY(U,$J,358.3,13750,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13750,1,4,0)
 ;;=4^519.11
 ;;^UTILITY(U,$J,358.3,13750,1,5,0)
 ;;=5^Acute Bronchospasm
 ;;^UTILITY(U,$J,358.3,13750,2)
 ;;=^334092
 ;;^UTILITY(U,$J,358.3,13751,0)
 ;;=488.01^^90^850^20
 ;;^UTILITY(U,$J,358.3,13751,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13751,1,4,0)
 ;;=4^488.01
 ;;^UTILITY(U,$J,358.3,13751,1,5,0)
 ;;=5^Flu DT Iden AVIAN w Pneu
 ;;^UTILITY(U,$J,358.3,13751,2)
 ;;=^339615
 ;;^UTILITY(U,$J,358.3,13752,0)
 ;;=488.02^^90^850^19
 ;;^UTILITY(U,$J,358.3,13752,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13752,1,4,0)
 ;;=4^488.02
 ;;^UTILITY(U,$J,358.3,13752,1,5,0)
 ;;=5^Flu DT AVIAN w oth Resp
 ;;^UTILITY(U,$J,358.3,13752,2)
 ;;=^339616
 ;;^UTILITY(U,$J,358.3,13753,0)
 ;;=488.09^^90^850^18
 ;;^UTILITY(U,$J,358.3,13753,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13753,1,4,0)
 ;;=4^488.09
 ;;^UTILITY(U,$J,358.3,13753,1,5,0)
 ;;=5^Flu DT AVIAN Manfest
 ;;^UTILITY(U,$J,358.3,13753,2)
 ;;=^339617
 ;;^UTILITY(U,$J,358.3,13754,0)
 ;;=786.30^^90^850^22
 ;;^UTILITY(U,$J,358.3,13754,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13754,1,4,0)
 ;;=4^786.30
 ;;^UTILITY(U,$J,358.3,13754,1,5,0)
 ;;=5^Hemoptysis
 ;;^UTILITY(U,$J,358.3,13754,2)
 ;;=^339669
 ;;^UTILITY(U,$J,358.3,13755,0)
 ;;=793.11^^90^850^1
 ;;^UTILITY(U,$J,358.3,13755,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13755,1,4,0)
 ;;=4^793.11
 ;;^UTILITY(U,$J,358.3,13755,1,5,0)
 ;;=5^Abn Chest Xray, Lung, Solitary Nodule
 ;;^UTILITY(U,$J,358.3,13755,2)
 ;;=^340570
 ;;^UTILITY(U,$J,358.3,13756,0)
 ;;=793.19^^90^850^2
 ;;^UTILITY(U,$J,358.3,13756,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13756,1,4,0)
 ;;=4^793.19
 ;;^UTILITY(U,$J,358.3,13756,1,5,0)
 ;;=5^Abn Chest Xray,Oth Finding, Lung
 ;;^UTILITY(U,$J,358.3,13756,2)
 ;;=^340571
 ;;^UTILITY(U,$J,358.3,13757,0)
 ;;=795.51^^90^850^32
 ;;^UTILITY(U,$J,358.3,13757,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13757,1,4,0)
 ;;=4^795.51
 ;;^UTILITY(U,$J,358.3,13757,1,5,0)
 ;;=5^Pos PPD w/o Active TB
 ;;^UTILITY(U,$J,358.3,13757,2)
 ;;=^340572
 ;;^UTILITY(U,$J,358.3,13758,0)
 ;;=795.52^^90^850^31
 ;;^UTILITY(U,$J,358.3,13758,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13758,1,4,0)
 ;;=4^795.52
 ;;^UTILITY(U,$J,358.3,13758,1,5,0)
 ;;=5^Pos GMA Interferon w/o Active TB
 ;;^UTILITY(U,$J,358.3,13758,2)
 ;;=^340573
 ;;^UTILITY(U,$J,358.3,13759,0)
 ;;=V15.82^^90^850^21
 ;;^UTILITY(U,$J,358.3,13759,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13759,1,4,0)
 ;;=4^V15.82
