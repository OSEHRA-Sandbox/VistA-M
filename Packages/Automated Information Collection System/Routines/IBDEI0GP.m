IBDEI0GP ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,16757,1,3,0)
 ;;=3^Pick's Disease
 ;;^UTILITY(U,$J,358.3,16757,1,4,0)
 ;;=4^G31.01
 ;;^UTILITY(U,$J,358.3,16757,2)
 ;;=^329915
 ;;^UTILITY(U,$J,358.3,16758,0)
 ;;=G23.1^^70^832^28
 ;;^UTILITY(U,$J,358.3,16758,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16758,1,3,0)
 ;;=3^Progressive Supranuclear Ophthalmoplegia
 ;;^UTILITY(U,$J,358.3,16758,1,4,0)
 ;;=4^G23.1
 ;;^UTILITY(U,$J,358.3,16758,2)
 ;;=^5003780
 ;;^UTILITY(U,$J,358.3,16759,0)
 ;;=99211^^71^833^1
 ;;^UTILITY(U,$J,358.3,16759,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16759,1,1,0)
 ;;=1^Brief Office Visit
 ;;^UTILITY(U,$J,358.3,16759,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,16760,0)
 ;;=99212^^71^833^2
 ;;^UTILITY(U,$J,358.3,16760,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16760,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,16760,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,16761,0)
 ;;=99213^^71^833^3
 ;;^UTILITY(U,$J,358.3,16761,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16761,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,16761,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,16762,0)
 ;;=99214^^71^833^4
 ;;^UTILITY(U,$J,358.3,16762,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16762,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,16762,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,16763,0)
 ;;=99215^^71^833^5
 ;;^UTILITY(U,$J,358.3,16763,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16763,1,1,0)
 ;;=1^Comprehensive
 ;;^UTILITY(U,$J,358.3,16763,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,16764,0)
 ;;=99201^^71^834^1
 ;;^UTILITY(U,$J,358.3,16764,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16764,1,1,0)
 ;;=1^Problem Focus
 ;;^UTILITY(U,$J,358.3,16764,1,2,0)
 ;;=2^99201
 ;;^UTILITY(U,$J,358.3,16765,0)
 ;;=99202^^71^834^2
 ;;^UTILITY(U,$J,358.3,16765,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16765,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,16765,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,16766,0)
 ;;=99203^^71^834^3
 ;;^UTILITY(U,$J,358.3,16766,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16766,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,16766,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,16767,0)
 ;;=99204^^71^834^4
 ;;^UTILITY(U,$J,358.3,16767,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16767,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,16767,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,16768,0)
 ;;=99205^^71^834^5
 ;;^UTILITY(U,$J,358.3,16768,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16768,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,16768,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,16769,0)
 ;;=99211^^71^835^1
 ;;^UTILITY(U,$J,358.3,16769,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16769,1,1,0)
 ;;=1^Brief Visit w/ mod
 ;;^UTILITY(U,$J,358.3,16769,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,16769,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16769,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16770,0)
 ;;=99212^^71^835^2
 ;;^UTILITY(U,$J,358.3,16770,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16770,1,1,0)
 ;;=1^Problem Focused w/ mod
 ;;^UTILITY(U,$J,358.3,16770,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,16770,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16770,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16771,0)
 ;;=99213^^71^835^3
 ;;^UTILITY(U,$J,358.3,16771,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16771,1,1,0)
 ;;=1^Expanded Problem Focus w/ mod
 ;;^UTILITY(U,$J,358.3,16771,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,16771,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16771,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16772,0)
 ;;=99214^^71^835^4
 ;;^UTILITY(U,$J,358.3,16772,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16772,1,1,0)
 ;;=1^Detailed w/ mod
 ;;^UTILITY(U,$J,358.3,16772,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,16772,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16772,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16773,0)
 ;;=99215^^71^835^5
 ;;^UTILITY(U,$J,358.3,16773,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16773,1,1,0)
 ;;=1^Comprehensive w/ mod
 ;;^UTILITY(U,$J,358.3,16773,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,16773,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16773,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16774,0)
 ;;=99201^^71^836^1
 ;;^UTILITY(U,$J,358.3,16774,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16774,1,1,0)
 ;;=1^Problem Focus w/ mod
 ;;^UTILITY(U,$J,358.3,16774,1,2,0)
 ;;=2^99201
 ;;^UTILITY(U,$J,358.3,16774,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16774,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16775,0)
 ;;=99202^^71^836^2
 ;;^UTILITY(U,$J,358.3,16775,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16775,1,1,0)
 ;;=1^Expanded Problem Focus w/ mod
 ;;^UTILITY(U,$J,358.3,16775,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,16775,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16775,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16776,0)
 ;;=99203^^71^836^3
 ;;^UTILITY(U,$J,358.3,16776,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16776,1,1,0)
 ;;=1^Detailed w/ mod
 ;;^UTILITY(U,$J,358.3,16776,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,16776,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16776,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16777,0)
 ;;=99204^^71^836^4
 ;;^UTILITY(U,$J,358.3,16777,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16777,1,1,0)
 ;;=1^Comprehensive, Moderate w/ mod
 ;;^UTILITY(U,$J,358.3,16777,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,16777,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16777,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16778,0)
 ;;=99205^^71^836^5
 ;;^UTILITY(U,$J,358.3,16778,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16778,1,1,0)
 ;;=1^Comprehensive, High w/ mod
 ;;^UTILITY(U,$J,358.3,16778,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,16778,3,0)
 ;;=^358.33^1^1
 ;;^UTILITY(U,$J,358.3,16778,3,1,0)
 ;;=GT
 ;;^UTILITY(U,$J,358.3,16779,0)
 ;;=99078^^72^837^4^^^^1
 ;;^UTILITY(U,$J,358.3,16779,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16779,1,2,0)
 ;;=2^99078
 ;;^UTILITY(U,$J,358.3,16779,1,3,0)
 ;;=3^GROUP HEALTH EDUCATION
 ;;^UTILITY(U,$J,358.3,16780,0)
 ;;=99080^^72^837^6^^^^1
 ;;^UTILITY(U,$J,358.3,16780,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16780,1,2,0)
 ;;=2^99080
 ;;^UTILITY(U,$J,358.3,16780,1,3,0)
 ;;=3^SPECIAL REPORTS OR FORMS
 ;;^UTILITY(U,$J,358.3,16781,0)
 ;;=99499^^72^837^7^^^^1
 ;;^UTILITY(U,$J,358.3,16781,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16781,1,2,0)
 ;;=2^99499
 ;;^UTILITY(U,$J,358.3,16781,1,3,0)
 ;;=3^UNLISTED E&M SERVICE
 ;;^UTILITY(U,$J,358.3,16782,0)
 ;;=G0108^^72^837^2^^^^1
 ;;^UTILITY(U,$J,358.3,16782,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16782,1,2,0)
 ;;=2^G0108
 ;;^UTILITY(U,$J,358.3,16782,1,3,0)
 ;;=3^DM MGMT TRN PER INDIV PER 30MIN
 ;;^UTILITY(U,$J,358.3,16783,0)
 ;;=G0109^^72^837^1^^^^1
 ;;^UTILITY(U,$J,358.3,16783,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16783,1,2,0)
 ;;=2^G0109
 ;;^UTILITY(U,$J,358.3,16783,1,3,0)
 ;;=3^DM MGMT TRN GRP PER 30MIN
 ;;^UTILITY(U,$J,358.3,16784,0)
 ;;=A9279^^72^837^5^^^^1
 ;;^UTILITY(U,$J,358.3,16784,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16784,1,2,0)
 ;;=2^A9279
 ;;^UTILITY(U,$J,358.3,16784,1,3,0)
 ;;=3^MONITORING FEATURE/DEVICE NOC
 ;;^UTILITY(U,$J,358.3,16785,0)
 ;;=93010^^72^837^3^^^^1
 ;;^UTILITY(U,$J,358.3,16785,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16785,1,2,0)
 ;;=2^93010
 ;;^UTILITY(U,$J,358.3,16785,1,3,0)
 ;;=3^EKG REPORT
 ;;^UTILITY(U,$J,358.3,16786,0)
 ;;=S9445^^72^838^1^^^^1
 ;;^UTILITY(U,$J,358.3,16786,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16786,1,2,0)
 ;;=2^S9445
 ;;^UTILITY(U,$J,358.3,16786,1,3,0)
 ;;=3^PT EDUCATION NOC INDIVID
 ;;^UTILITY(U,$J,358.3,16787,0)
 ;;=99091^^72^839^1^^^^1
 ;;^UTILITY(U,$J,358.3,16787,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16787,1,2,0)
 ;;=2^99091
 ;;^UTILITY(U,$J,358.3,16787,1,3,0)
 ;;=3^COLLECT/REVIEW DATA FROM PT
 ;;^UTILITY(U,$J,358.3,16788,0)
 ;;=99347^^72^840^1^^^^1
 ;;^UTILITY(U,$J,358.3,16788,1,0)
 ;;=^358.31IA^3^2
