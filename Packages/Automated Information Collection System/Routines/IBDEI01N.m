IBDEI01N ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,292,0)
 ;;=V62.84^^2^19^39
 ;;^UTILITY(U,$J,358.3,292,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,292,1,2,0)
 ;;=2^V62.84
 ;;^UTILITY(U,$J,358.3,292,1,5,0)
 ;;=5^Suicidal Ideation
 ;;^UTILITY(U,$J,358.3,292,2)
 ;;=^332876
 ;;^UTILITY(U,$J,358.3,293,0)
 ;;=V62.85^^2^19^16
 ;;^UTILITY(U,$J,358.3,293,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,293,1,2,0)
 ;;=2^V62.85
 ;;^UTILITY(U,$J,358.3,293,1,5,0)
 ;;=5^Homicidal Ideation
 ;;^UTILITY(U,$J,358.3,293,2)
 ;;=^339690
 ;;^UTILITY(U,$J,358.3,294,0)
 ;;=V79.1^^2^20^1
 ;;^UTILITY(U,$J,358.3,294,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,294,1,2,0)
 ;;=2^V79.1
 ;;^UTILITY(U,$J,358.3,294,1,5,0)
 ;;=5^Alcohol Screen
 ;;^UTILITY(U,$J,358.3,294,2)
 ;;=^295678
 ;;^UTILITY(U,$J,358.3,295,0)
 ;;=V61.21^^2^20^8
 ;;^UTILITY(U,$J,358.3,295,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,295,1,2,0)
 ;;=2^V61.21
 ;;^UTILITY(U,$J,358.3,295,1,5,0)
 ;;=5^Victim Child Abuse Counseling
 ;;^UTILITY(U,$J,358.3,295,2)
 ;;=^304301
 ;;^UTILITY(U,$J,358.3,296,0)
 ;;=V65.40^^2^20^2
 ;;^UTILITY(U,$J,358.3,296,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,296,1,2,0)
 ;;=2^V65.40
 ;;^UTILITY(U,$J,358.3,296,1,5,0)
 ;;=5^Counseling NOS
 ;;^UTILITY(U,$J,358.3,296,2)
 ;;=^87449
 ;;^UTILITY(U,$J,358.3,297,0)
 ;;=V65.42^^2^20^7
 ;;^UTILITY(U,$J,358.3,297,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,297,1,2,0)
 ;;=2^V65.42
 ;;^UTILITY(U,$J,358.3,297,1,5,0)
 ;;=5^Substance Use/Abuse Counseling
 ;;^UTILITY(U,$J,358.3,297,2)
 ;;=^303467
 ;;^UTILITY(U,$J,358.3,298,0)
 ;;=V65.44^^2^20^3
 ;;^UTILITY(U,$J,358.3,298,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,298,1,2,0)
 ;;=2^V65.44
 ;;^UTILITY(U,$J,358.3,298,1,5,0)
 ;;=5^HIV Counseling
 ;;^UTILITY(U,$J,358.3,298,2)
 ;;=^303469
 ;;^UTILITY(U,$J,358.3,299,0)
 ;;=V65.49^^2^20^5
 ;;^UTILITY(U,$J,358.3,299,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,299,1,2,0)
 ;;=2^V65.49
 ;;^UTILITY(U,$J,358.3,299,1,5,0)
 ;;=5^Oth Specified Counseling
 ;;^UTILITY(U,$J,358.3,299,2)
 ;;=^303471
 ;;^UTILITY(U,$J,358.3,300,0)
 ;;=V68.1^^2^20^4
 ;;^UTILITY(U,$J,358.3,300,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,300,1,2,0)
 ;;=2^V68.1
 ;;^UTILITY(U,$J,358.3,300,1,5,0)
 ;;=5^Issue Repeat Prescription
 ;;^UTILITY(U,$J,358.3,300,2)
 ;;=^295585
 ;;^UTILITY(U,$J,358.3,301,0)
 ;;=V70.2^^2^20^6
 ;;^UTILITY(U,$J,358.3,301,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,301,1,2,0)
 ;;=2^V70.2
 ;;^UTILITY(U,$J,358.3,301,1,5,0)
 ;;=5^Psychiatric Examination
 ;;^UTILITY(U,$J,358.3,301,2)
 ;;=^295592
 ;;^UTILITY(U,$J,358.3,302,0)
 ;;=296.20^^2^21^14
 ;;^UTILITY(U,$J,358.3,302,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,302,1,2,0)
 ;;=2^296.20
 ;;^UTILITY(U,$J,358.3,302,1,5,0)
 ;;=5^MDD, Single, NOS
 ;;^UTILITY(U,$J,358.3,302,2)
 ;;=^73311
 ;;^UTILITY(U,$J,358.3,303,0)
 ;;=296.21^^2^21^12
 ;;^UTILITY(U,$J,358.3,303,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,303,1,2,0)
 ;;=2^296.21
 ;;^UTILITY(U,$J,358.3,303,1,5,0)
 ;;=5^MDD, Single, Mild
 ;;^UTILITY(U,$J,358.3,303,2)
 ;;=^268110
 ;;^UTILITY(U,$J,358.3,304,0)
 ;;=296.22^^2^21^13
 ;;^UTILITY(U,$J,358.3,304,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,304,1,2,0)
 ;;=2^296.22
 ;;^UTILITY(U,$J,358.3,304,1,5,0)
 ;;=5^MDD, Single, Moderate
 ;;^UTILITY(U,$J,358.3,304,2)
 ;;=^268111
 ;;^UTILITY(U,$J,358.3,305,0)
 ;;=296.23^^2^21^5
 ;;^UTILITY(U,$J,358.3,305,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,305,1,2,0)
 ;;=2^296.23
 ;;^UTILITY(U,$J,358.3,305,1,5,0)
 ;;=5^MDD Sing, Sev w/o Psychosis
 ;;^UTILITY(U,$J,358.3,305,2)
 ;;=^268112
 ;;^UTILITY(U,$J,358.3,306,0)
 ;;=296.24^^2^21^4
 ;;^UTILITY(U,$J,358.3,306,1,0)
 ;;=^358.31IA^5^2
