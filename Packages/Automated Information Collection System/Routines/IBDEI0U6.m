IBDEI0U6 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13889,1,4,0)
 ;;=4^H10.13
 ;;^UTILITY(U,$J,358.3,13889,2)
 ;;=^5004667
 ;;^UTILITY(U,$J,358.3,13890,0)
 ;;=H10.211^^82^769^159
 ;;^UTILITY(U,$J,358.3,13890,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13890,1,3,0)
 ;;=3^Toxic Conjunctivitis,Right Eye,Acute
 ;;^UTILITY(U,$J,358.3,13890,1,4,0)
 ;;=4^H10.211
 ;;^UTILITY(U,$J,358.3,13890,2)
 ;;=^5004668
 ;;^UTILITY(U,$J,358.3,13891,0)
 ;;=H10.212^^82^769^158
 ;;^UTILITY(U,$J,358.3,13891,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13891,1,3,0)
 ;;=3^Toxic Conjunctivitis,Left Eye,Acute
 ;;^UTILITY(U,$J,358.3,13891,1,4,0)
 ;;=4^H10.212
 ;;^UTILITY(U,$J,358.3,13891,2)
 ;;=^5004669
 ;;^UTILITY(U,$J,358.3,13892,0)
 ;;=H10.401^^82^769^46
 ;;^UTILITY(U,$J,358.3,13892,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13892,1,3,0)
 ;;=3^Conjunctivitis,Right Eye,Chronic
 ;;^UTILITY(U,$J,358.3,13892,1,4,0)
 ;;=4^H10.401
 ;;^UTILITY(U,$J,358.3,13892,2)
 ;;=^5004681
 ;;^UTILITY(U,$J,358.3,13893,0)
 ;;=H10.402^^82^769^45
 ;;^UTILITY(U,$J,358.3,13893,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13893,1,3,0)
 ;;=3^Conjunctivitis,Left Eye,Chronic
 ;;^UTILITY(U,$J,358.3,13893,1,4,0)
 ;;=4^H10.402
 ;;^UTILITY(U,$J,358.3,13893,2)
 ;;=^5004682
 ;;^UTILITY(U,$J,358.3,13894,0)
 ;;=H10.411^^82^769^79
 ;;^UTILITY(U,$J,358.3,13894,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13894,1,3,0)
 ;;=3^Giant Papillary Conjunctivitis,Right Eye,Chronic
 ;;^UTILITY(U,$J,358.3,13894,1,4,0)
 ;;=4^H10.411
 ;;^UTILITY(U,$J,358.3,13894,2)
 ;;=^5004684
 ;;^UTILITY(U,$J,358.3,13895,0)
 ;;=H10.412^^82^769^78
 ;;^UTILITY(U,$J,358.3,13895,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13895,1,3,0)
 ;;=3^Giant Papillary Conjunctivitis,Left Eye,Chronic
 ;;^UTILITY(U,$J,358.3,13895,1,4,0)
 ;;=4^H10.412
 ;;^UTILITY(U,$J,358.3,13895,2)
 ;;=^5004685
 ;;^UTILITY(U,$J,358.3,13896,0)
 ;;=H10.45^^82^769^1
 ;;^UTILITY(U,$J,358.3,13896,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13896,1,3,0)
 ;;=3^Allergic Conjunctivitis,Chronic NEC
 ;;^UTILITY(U,$J,358.3,13896,1,4,0)
 ;;=4^H10.45
 ;;^UTILITY(U,$J,358.3,13896,2)
 ;;=^87396
 ;;^UTILITY(U,$J,358.3,13897,0)
 ;;=H10.501^^82^769^21
 ;;^UTILITY(U,$J,358.3,13897,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13897,1,3,0)
 ;;=3^Blepharoconjunctivitis,Right Eye,Unspec
 ;;^UTILITY(U,$J,358.3,13897,1,4,0)
 ;;=4^H10.501
 ;;^UTILITY(U,$J,358.3,13897,2)
 ;;=^5004697
 ;;^UTILITY(U,$J,358.3,13898,0)
 ;;=H10.502^^82^769^20
 ;;^UTILITY(U,$J,358.3,13898,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13898,1,3,0)
 ;;=3^Blepharoconjunctivitis,Left Eye,Unspec
 ;;^UTILITY(U,$J,358.3,13898,1,4,0)
 ;;=4^H10.502
 ;;^UTILITY(U,$J,358.3,13898,2)
 ;;=^5004698
 ;;^UTILITY(U,$J,358.3,13899,0)
 ;;=H11.011^^82^769^4
 ;;^UTILITY(U,$J,358.3,13899,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13899,1,3,0)
 ;;=3^Amyloid Pterygium,Right Eye
 ;;^UTILITY(U,$J,358.3,13899,1,4,0)
 ;;=4^H11.011
 ;;^UTILITY(U,$J,358.3,13899,2)
 ;;=^5004720
 ;;^UTILITY(U,$J,358.3,13900,0)
 ;;=H11.012^^82^769^3
 ;;^UTILITY(U,$J,358.3,13900,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13900,1,3,0)
 ;;=3^Amyloid Pterygium,Left Eye
 ;;^UTILITY(U,$J,358.3,13900,1,4,0)
 ;;=4^H11.012
 ;;^UTILITY(U,$J,358.3,13900,2)
 ;;=^5004721
 ;;^UTILITY(U,$J,358.3,13901,0)
 ;;=H11.002^^82^769^140
 ;;^UTILITY(U,$J,358.3,13901,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13901,1,3,0)
 ;;=3^Pterygium,Left Eye,Unspec
 ;;^UTILITY(U,$J,358.3,13901,1,4,0)
 ;;=4^H11.002
 ;;^UTILITY(U,$J,358.3,13901,2)
 ;;=^5004718
 ;;^UTILITY(U,$J,358.3,13902,0)
 ;;=H11.001^^82^769^141
 ;;^UTILITY(U,$J,358.3,13902,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13902,1,3,0)
 ;;=3^Pterygium,Right Eye,Unspec
