IBDEI0R2 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,12390,2)
 ;;=^5011611
 ;;^UTILITY(U,$J,358.3,12391,0)
 ;;=M25.552^^71^709^33
 ;;^UTILITY(U,$J,358.3,12391,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12391,1,3,0)
 ;;=3^Pain in left hip
 ;;^UTILITY(U,$J,358.3,12391,1,4,0)
 ;;=4^M25.552
 ;;^UTILITY(U,$J,358.3,12391,2)
 ;;=^5011612
 ;;^UTILITY(U,$J,358.3,12392,0)
 ;;=M25.561^^71^709^37
 ;;^UTILITY(U,$J,358.3,12392,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12392,1,3,0)
 ;;=3^Pain in right knee
 ;;^UTILITY(U,$J,358.3,12392,1,4,0)
 ;;=4^M25.561
 ;;^UTILITY(U,$J,358.3,12392,2)
 ;;=^5011614
 ;;^UTILITY(U,$J,358.3,12393,0)
 ;;=M25.562^^71^709^34
 ;;^UTILITY(U,$J,358.3,12393,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12393,1,3,0)
 ;;=3^Pain in left knee
 ;;^UTILITY(U,$J,358.3,12393,1,4,0)
 ;;=4^M25.562
 ;;^UTILITY(U,$J,358.3,12393,2)
 ;;=^5011615
 ;;^UTILITY(U,$J,358.3,12394,0)
 ;;=M54.2^^71^709^9
 ;;^UTILITY(U,$J,358.3,12394,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12394,1,3,0)
 ;;=3^Cervicalgia
 ;;^UTILITY(U,$J,358.3,12394,1,4,0)
 ;;=4^M54.2
 ;;^UTILITY(U,$J,358.3,12394,2)
 ;;=^5012304
 ;;^UTILITY(U,$J,358.3,12395,0)
 ;;=M54.5^^71^709^22
 ;;^UTILITY(U,$J,358.3,12395,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12395,1,3,0)
 ;;=3^Low back pain
 ;;^UTILITY(U,$J,358.3,12395,1,4,0)
 ;;=4^M54.5
 ;;^UTILITY(U,$J,358.3,12395,2)
 ;;=^5012311
 ;;^UTILITY(U,$J,358.3,12396,0)
 ;;=M54.9^^71^709^15
 ;;^UTILITY(U,$J,358.3,12396,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12396,1,3,0)
 ;;=3^Dorsalgia, unspecified
 ;;^UTILITY(U,$J,358.3,12396,1,4,0)
 ;;=4^M54.9
 ;;^UTILITY(U,$J,358.3,12396,2)
 ;;=^5012314
 ;;^UTILITY(U,$J,358.3,12397,0)
 ;;=R51.^^71^709^18
 ;;^UTILITY(U,$J,358.3,12397,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12397,1,3,0)
 ;;=3^Headache
 ;;^UTILITY(U,$J,358.3,12397,1,4,0)
 ;;=4^R51.
 ;;^UTILITY(U,$J,358.3,12397,2)
 ;;=^5019513
 ;;^UTILITY(U,$J,358.3,12398,0)
 ;;=R05.^^71^709^14
 ;;^UTILITY(U,$J,358.3,12398,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12398,1,3,0)
 ;;=3^Cough
 ;;^UTILITY(U,$J,358.3,12398,1,4,0)
 ;;=4^R05.
 ;;^UTILITY(U,$J,358.3,12398,2)
 ;;=^5019179
 ;;^UTILITY(U,$J,358.3,12399,0)
 ;;=R07.9^^71^709^10
 ;;^UTILITY(U,$J,358.3,12399,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12399,1,3,0)
 ;;=3^Chest pain, unspecified
 ;;^UTILITY(U,$J,358.3,12399,1,4,0)
 ;;=4^R07.9
 ;;^UTILITY(U,$J,358.3,12399,2)
 ;;=^5019201
 ;;^UTILITY(U,$J,358.3,12400,0)
 ;;=R10.9^^71^709^1
 ;;^UTILITY(U,$J,358.3,12400,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12400,1,3,0)
 ;;=3^Abdominal Pain,Unspec
 ;;^UTILITY(U,$J,358.3,12400,1,4,0)
 ;;=4^R10.9
 ;;^UTILITY(U,$J,358.3,12400,2)
 ;;=^5019230
 ;;^UTILITY(U,$J,358.3,12401,0)
 ;;=R76.11^^71^709^31
 ;;^UTILITY(U,$J,358.3,12401,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12401,1,3,0)
 ;;=3^Nonspecific reaction to skin test w/o active tuberculosis
 ;;^UTILITY(U,$J,358.3,12401,1,4,0)
 ;;=4^R76.11
 ;;^UTILITY(U,$J,358.3,12401,2)
 ;;=^5019570
 ;;^UTILITY(U,$J,358.3,12402,0)
 ;;=R76.12^^71^709^30
 ;;^UTILITY(U,$J,358.3,12402,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12402,1,3,0)
 ;;=3^Nonspec reaction to gamma intrfrn respns w/o actv tubrclosis
 ;;^UTILITY(U,$J,358.3,12402,1,4,0)
 ;;=4^R76.12
 ;;^UTILITY(U,$J,358.3,12402,2)
 ;;=^5019571
 ;;^UTILITY(U,$J,358.3,12403,0)
 ;;=R03.0^^71^709^16
 ;;^UTILITY(U,$J,358.3,12403,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,12403,1,3,0)
 ;;=3^Elevated blood-pressure reading, w/o diagnosis of htn
 ;;^UTILITY(U,$J,358.3,12403,1,4,0)
 ;;=4^R03.0
 ;;^UTILITY(U,$J,358.3,12403,2)
 ;;=^5019171
 ;;^UTILITY(U,$J,358.3,12404,0)
 ;;=M25.50^^71^709^32
 ;;^UTILITY(U,$J,358.3,12404,1,0)
 ;;=^358.31IA^4^2
