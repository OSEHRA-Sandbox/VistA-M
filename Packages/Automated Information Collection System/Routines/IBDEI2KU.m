IBDEI2KU ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,43224,0)
 ;;=99407^^196^2176^2^^^^1
 ;;^UTILITY(U,$J,358.3,43224,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43224,1,2,0)
 ;;=2^99407
 ;;^UTILITY(U,$J,358.3,43224,1,3,0)
 ;;=3^Tobacco Use Cessation,Intensive > 10 min
 ;;^UTILITY(U,$J,358.3,43225,0)
 ;;=99408^^196^2176^3^^^^1
 ;;^UTILITY(U,$J,358.3,43225,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43225,1,2,0)
 ;;=2^99408
 ;;^UTILITY(U,$J,358.3,43225,1,3,0)
 ;;=3^Alcohol/Subs Abuse Scrn/Intervention 15-30 min
 ;;^UTILITY(U,$J,358.3,43226,0)
 ;;=99409^^196^2176^4^^^^1
 ;;^UTILITY(U,$J,358.3,43226,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43226,1,2,0)
 ;;=2^99409
 ;;^UTILITY(U,$J,358.3,43226,1,3,0)
 ;;=3^Alcohol/Subs Abuse Scrn/Intervention > 30 min
 ;;^UTILITY(U,$J,358.3,43227,0)
 ;;=99401^^196^2177^1^^^^1
 ;;^UTILITY(U,$J,358.3,43227,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43227,1,2,0)
 ;;=2^99401
 ;;^UTILITY(U,$J,358.3,43227,1,3,0)
 ;;=3^Brief Counseling,15 Min
 ;;^UTILITY(U,$J,358.3,43228,0)
 ;;=99402^^196^2177^2^^^^1
 ;;^UTILITY(U,$J,358.3,43228,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43228,1,2,0)
 ;;=2^99402
 ;;^UTILITY(U,$J,358.3,43228,1,3,0)
 ;;=3^Intermediate Counseling,30 Min
 ;;^UTILITY(U,$J,358.3,43229,0)
 ;;=99403^^196^2177^3^^^^1
 ;;^UTILITY(U,$J,358.3,43229,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43229,1,2,0)
 ;;=2^99403
 ;;^UTILITY(U,$J,358.3,43229,1,3,0)
 ;;=3^Extended Counseling,45 Min
 ;;^UTILITY(U,$J,358.3,43230,0)
 ;;=99404^^196^2177^4^^^^1
 ;;^UTILITY(U,$J,358.3,43230,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43230,1,2,0)
 ;;=2^99404
 ;;^UTILITY(U,$J,358.3,43230,1,3,0)
 ;;=3^Comprehensive Counseling,60 Min
 ;;^UTILITY(U,$J,358.3,43231,0)
 ;;=99420^^196^2178^1^^^^1
 ;;^UTILITY(U,$J,358.3,43231,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43231,1,2,0)
 ;;=2^99420
 ;;^UTILITY(U,$J,358.3,43231,1,3,0)
 ;;=3^Admin/Intrp Hlth Risk Assess
 ;;^UTILITY(U,$J,358.3,43232,0)
 ;;=99411^^196^2179^1^^^^1
 ;;^UTILITY(U,$J,358.3,43232,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43232,1,2,0)
 ;;=2^99411
 ;;^UTILITY(U,$J,358.3,43232,1,3,0)
 ;;=3^Brief Counseling,Grp,30 Min
 ;;^UTILITY(U,$J,358.3,43233,0)
 ;;=99412^^196^2179^2^^^^1
 ;;^UTILITY(U,$J,358.3,43233,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43233,1,2,0)
 ;;=2^99412
 ;;^UTILITY(U,$J,358.3,43233,1,3,0)
 ;;=3^Intermediate Counseling,Grp,60 Min
 ;;^UTILITY(U,$J,358.3,43234,0)
 ;;=99395^^196^2180^1^^^^1
 ;;^UTILITY(U,$J,358.3,43234,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43234,1,2,0)
 ;;=2^99395
 ;;^UTILITY(U,$J,358.3,43234,1,3,0)
 ;;=3^Preventive Hlth Visit,Est Pt 18-39
 ;;^UTILITY(U,$J,358.3,43235,0)
 ;;=99396^^196^2180^2^^^^1
 ;;^UTILITY(U,$J,358.3,43235,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43235,1,2,0)
 ;;=2^99396
 ;;^UTILITY(U,$J,358.3,43235,1,3,0)
 ;;=3^Preventive Hlth Visit,Est Pt 40-64
 ;;^UTILITY(U,$J,358.3,43236,0)
 ;;=99397^^196^2180^3^^^^1
 ;;^UTILITY(U,$J,358.3,43236,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43236,1,2,0)
 ;;=2^99397
 ;;^UTILITY(U,$J,358.3,43236,1,3,0)
 ;;=3^Preventive Hlth Visit,Est Pt > 64
 ;;^UTILITY(U,$J,358.3,43237,0)
 ;;=99385^^196^2181^1^^^^1
 ;;^UTILITY(U,$J,358.3,43237,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43237,1,2,0)
 ;;=2^99385
 ;;^UTILITY(U,$J,358.3,43237,1,3,0)
 ;;=3^Preventive Hlth Visit,New Pt 18-39
 ;;^UTILITY(U,$J,358.3,43238,0)
 ;;=99386^^196^2181^2^^^^1
 ;;^UTILITY(U,$J,358.3,43238,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,43238,1,2,0)
 ;;=2^99386
 ;;^UTILITY(U,$J,358.3,43238,1,3,0)
 ;;=3^Preventive Hlth Visit,New Pt 40-64
 ;;^UTILITY(U,$J,358.3,43239,0)
 ;;=99387^^196^2181^3^^^^1
