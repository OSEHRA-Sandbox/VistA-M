IBDEI14V ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,19284,1,4,0)
 ;;=4^I25.110
 ;;^UTILITY(U,$J,358.3,19284,2)
 ;;=^5007108
 ;;^UTILITY(U,$J,358.3,19285,0)
 ;;=I25.700^^84^915^12
 ;;^UTILITY(U,$J,358.3,19285,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19285,1,3,0)
 ;;=3^Athscl of CABG,Unspec w/ Unstable Angina Pectoris
 ;;^UTILITY(U,$J,358.3,19285,1,4,0)
 ;;=4^I25.700
 ;;^UTILITY(U,$J,358.3,19285,2)
 ;;=^5007117
 ;;^UTILITY(U,$J,358.3,19286,0)
 ;;=I25.2^^84^915^13
 ;;^UTILITY(U,$J,358.3,19286,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19286,1,3,0)
 ;;=3^Old Myocardial Infarction
 ;;^UTILITY(U,$J,358.3,19286,1,4,0)
 ;;=4^I25.2
 ;;^UTILITY(U,$J,358.3,19286,2)
 ;;=^259884
 ;;^UTILITY(U,$J,358.3,19287,0)
 ;;=I20.8^^84^915^2
 ;;^UTILITY(U,$J,358.3,19287,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19287,1,3,0)
 ;;=3^Angina Pectoris,Oth Forms
 ;;^UTILITY(U,$J,358.3,19287,1,4,0)
 ;;=4^I20.8
 ;;^UTILITY(U,$J,358.3,19287,2)
 ;;=^5007078
 ;;^UTILITY(U,$J,358.3,19288,0)
 ;;=I20.1^^84^915^1
 ;;^UTILITY(U,$J,358.3,19288,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19288,1,3,0)
 ;;=3^Angina Pectoris w/ Documented Spasm
 ;;^UTILITY(U,$J,358.3,19288,1,4,0)
 ;;=4^I20.1
 ;;^UTILITY(U,$J,358.3,19288,2)
 ;;=^5007077
 ;;^UTILITY(U,$J,358.3,19289,0)
 ;;=I25.119^^84^915^5
 ;;^UTILITY(U,$J,358.3,19289,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19289,1,3,0)
 ;;=3^Athscl HRT Dis of Native Cor Art w/ Ang Pctrs,Unspec
 ;;^UTILITY(U,$J,358.3,19289,1,4,0)
 ;;=4^I25.119
 ;;^UTILITY(U,$J,358.3,19289,2)
 ;;=^5007111
 ;;^UTILITY(U,$J,358.3,19290,0)
 ;;=I25.701^^84^915^9
 ;;^UTILITY(U,$J,358.3,19290,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19290,1,3,0)
 ;;=3^Athscl of CABG,Unspec w/ Angina Pectoris w/ Documented Spasm
 ;;^UTILITY(U,$J,358.3,19290,1,4,0)
 ;;=4^I25.701
 ;;^UTILITY(U,$J,358.3,19290,2)
 ;;=^5007118
 ;;^UTILITY(U,$J,358.3,19291,0)
 ;;=I25.708^^84^915^10
 ;;^UTILITY(U,$J,358.3,19291,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19291,1,3,0)
 ;;=3^Athscl of CABG,Unspec w/ Oth Angina Pectoris
 ;;^UTILITY(U,$J,358.3,19291,1,4,0)
 ;;=4^I25.708
 ;;^UTILITY(U,$J,358.3,19291,2)
 ;;=^5007119
 ;;^UTILITY(U,$J,358.3,19292,0)
 ;;=I20.9^^84^915^3
 ;;^UTILITY(U,$J,358.3,19292,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19292,1,3,0)
 ;;=3^Angina Pectoris,Unspec
 ;;^UTILITY(U,$J,358.3,19292,1,4,0)
 ;;=4^I20.9
 ;;^UTILITY(U,$J,358.3,19292,2)
 ;;=^5007079
 ;;^UTILITY(U,$J,358.3,19293,0)
 ;;=I25.729^^84^915^4
 ;;^UTILITY(U,$J,358.3,19293,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19293,1,3,0)
 ;;=3^Athscl Autologous Artery CABG w/ Unspec Angina Pectoris
 ;;^UTILITY(U,$J,358.3,19293,1,4,0)
 ;;=4^I25.729
 ;;^UTILITY(U,$J,358.3,19293,2)
 ;;=^5133561
 ;;^UTILITY(U,$J,358.3,19294,0)
 ;;=I25.709^^84^915^11
 ;;^UTILITY(U,$J,358.3,19294,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19294,1,3,0)
 ;;=3^Athscl of CABG,Unspec w/ Unspec Angina Pectoris
 ;;^UTILITY(U,$J,358.3,19294,1,4,0)
 ;;=4^I25.709
 ;;^UTILITY(U,$J,358.3,19294,2)
 ;;=^5007120
 ;;^UTILITY(U,$J,358.3,19295,0)
 ;;=I25.10^^84^915^6
 ;;^UTILITY(U,$J,358.3,19295,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19295,1,3,0)
 ;;=3^Athscl HRT Dis of Native Cor Art w/o Ang Pctrs
 ;;^UTILITY(U,$J,358.3,19295,1,4,0)
 ;;=4^I25.10
 ;;^UTILITY(U,$J,358.3,19295,2)
 ;;=^5007107
 ;;^UTILITY(U,$J,358.3,19296,0)
 ;;=I25.810^^84^915^8
 ;;^UTILITY(U,$J,358.3,19296,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19296,1,3,0)
 ;;=3^Athscl of CABG w/o Angina Pectoris
 ;;^UTILITY(U,$J,358.3,19296,1,4,0)
 ;;=4^I25.810
 ;;^UTILITY(U,$J,358.3,19296,2)
 ;;=^5007141
 ;;^UTILITY(U,$J,358.3,19297,0)
 ;;=I65.29^^84^916^18
 ;;^UTILITY(U,$J,358.3,19297,1,0)
 ;;=^358.31IA^4^2
