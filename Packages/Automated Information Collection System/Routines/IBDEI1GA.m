IBDEI1GA ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,24227,0)
 ;;=99091^^117^1192^1^^^^1
 ;;^UTILITY(U,$J,358.3,24227,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24227,1,2,0)
 ;;=2^99091
 ;;^UTILITY(U,$J,358.3,24227,1,3,0)
 ;;=3^COLLECT/REVIEW DATA FROM PT
 ;;^UTILITY(U,$J,358.3,24228,0)
 ;;=G0155^^117^1193^1^^^^1
 ;;^UTILITY(U,$J,358.3,24228,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24228,1,2,0)
 ;;=2^G0155
 ;;^UTILITY(U,$J,358.3,24228,1,3,0)
 ;;=3^HOME VISIT BY CSW,EA 15 MIN
 ;;^UTILITY(U,$J,358.3,24229,0)
 ;;=90847^^117^1194^1^^^^1
 ;;^UTILITY(U,$J,358.3,24229,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24229,1,2,0)
 ;;=2^90847
 ;;^UTILITY(U,$J,358.3,24229,1,3,0)
 ;;=3^FAMILY PSYTX W/PATIENT
 ;;^UTILITY(U,$J,358.3,24230,0)
 ;;=90849^^117^1194^3^^^^1
 ;;^UTILITY(U,$J,358.3,24230,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24230,1,2,0)
 ;;=2^90849
 ;;^UTILITY(U,$J,358.3,24230,1,3,0)
 ;;=3^MULTIPLE FAMILY GROUP PSYTX
 ;;^UTILITY(U,$J,358.3,24231,0)
 ;;=90853^^117^1194^2^^^^1
 ;;^UTILITY(U,$J,358.3,24231,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24231,1,2,0)
 ;;=2^90853
 ;;^UTILITY(U,$J,358.3,24231,1,3,0)
 ;;=3^GROUP PSYCHOTHERAPY
 ;;^UTILITY(U,$J,358.3,24232,0)
 ;;=90832^^117^1194^4^^^^1
 ;;^UTILITY(U,$J,358.3,24232,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24232,1,2,0)
 ;;=2^90832
 ;;^UTILITY(U,$J,358.3,24232,1,3,0)
 ;;=3^PSYTX PT OR FAMILY 17-32 MIN
 ;;^UTILITY(U,$J,358.3,24233,0)
 ;;=90834^^117^1194^5^^^^1
 ;;^UTILITY(U,$J,358.3,24233,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24233,1,2,0)
 ;;=2^90834
 ;;^UTILITY(U,$J,358.3,24233,1,3,0)
 ;;=3^PSYTX PT OR FAMILY 38-52 MIN
 ;;^UTILITY(U,$J,358.3,24234,0)
 ;;=90837^^117^1194^6^^^^1
 ;;^UTILITY(U,$J,358.3,24234,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24234,1,2,0)
 ;;=2^90837
 ;;^UTILITY(U,$J,358.3,24234,1,3,0)
 ;;=3^PSYTX PT OR FAMILY 53+ MIN
 ;;^UTILITY(U,$J,358.3,24235,0)
 ;;=T1016^^117^1195^1^^^^1
 ;;^UTILITY(U,$J,358.3,24235,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,24235,1,2,0)
 ;;=2^T1016
 ;;^UTILITY(U,$J,358.3,24235,1,3,0)
 ;;=3^CASE MANAGEMENT EA 15MIN
 ;;^UTILITY(U,$J,358.3,24236,0)
 ;;=Z02.71^^118^1196^2
 ;;^UTILITY(U,$J,358.3,24236,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24236,1,3,0)
 ;;=3^Disability Determination Exam
 ;;^UTILITY(U,$J,358.3,24236,1,4,0)
 ;;=4^Z02.71
 ;;^UTILITY(U,$J,358.3,24236,2)
 ;;=^5062640
 ;;^UTILITY(U,$J,358.3,24237,0)
 ;;=Z02.79^^118^1196^4
 ;;^UTILITY(U,$J,358.3,24237,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24237,1,3,0)
 ;;=3^Issue of Medical Certificate
 ;;^UTILITY(U,$J,358.3,24237,1,4,0)
 ;;=4^Z02.79
 ;;^UTILITY(U,$J,358.3,24237,2)
 ;;=^5062641
 ;;^UTILITY(U,$J,358.3,24238,0)
 ;;=Z04.9^^118^1196^3
 ;;^UTILITY(U,$J,358.3,24238,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24238,1,3,0)
 ;;=3^Exam & Observation for Unspec Reason
 ;;^UTILITY(U,$J,358.3,24238,1,4,0)
 ;;=4^Z04.9
 ;;^UTILITY(U,$J,358.3,24238,2)
 ;;=^5062666
 ;;^UTILITY(U,$J,358.3,24239,0)
 ;;=Z02.9^^118^1196^1
 ;;^UTILITY(U,$J,358.3,24239,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24239,1,3,0)
 ;;=3^Adminstrative Exams,Unspec
 ;;^UTILITY(U,$J,358.3,24239,1,4,0)
 ;;=4^Z02.9
 ;;^UTILITY(U,$J,358.3,24239,2)
 ;;=^5062646
 ;;^UTILITY(U,$J,358.3,24240,0)
 ;;=Z76.82^^118^1197^2
 ;;^UTILITY(U,$J,358.3,24240,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24240,1,3,0)
 ;;=3^Awaiting Organ Transplant Status
 ;;^UTILITY(U,$J,358.3,24240,1,4,0)
 ;;=4^Z76.82
 ;;^UTILITY(U,$J,358.3,24240,2)
 ;;=^331582
 ;;^UTILITY(U,$J,358.3,24241,0)
 ;;=Z78.0^^118^1197^1
 ;;^UTILITY(U,$J,358.3,24241,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24241,1,3,0)
 ;;=3^Asymptomatic Menopausal State
