IBDEI0OM ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,12136,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12136,1,4,0)
 ;;=4^578.1
 ;;^UTILITY(U,$J,358.3,12136,1,5,0)
 ;;=5^Melena
 ;;^UTILITY(U,$J,358.3,12136,2)
 ;;=^276839
 ;;^UTILITY(U,$J,358.3,12137,0)
 ;;=571.5^^84^801^8
 ;;^UTILITY(U,$J,358.3,12137,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12137,1,4,0)
 ;;=4^571.5
 ;;^UTILITY(U,$J,358.3,12137,1,5,0)
 ;;=5^Cirrhosis
 ;;^UTILITY(U,$J,358.3,12137,2)
 ;;=^24731
 ;;^UTILITY(U,$J,358.3,12138,0)
 ;;=558.9^^84^801^24
 ;;^UTILITY(U,$J,358.3,12138,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12138,1,4,0)
 ;;=4^558.9
 ;;^UTILITY(U,$J,358.3,12138,1,5,0)
 ;;=5^Inflammatory Bowel Disease
 ;;^UTILITY(U,$J,358.3,12138,2)
 ;;=^87311
 ;;^UTILITY(U,$J,358.3,12139,0)
 ;;=555.9^^84^801^10
 ;;^UTILITY(U,$J,358.3,12139,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12139,1,4,0)
 ;;=4^555.9
 ;;^UTILITY(U,$J,358.3,12139,1,5,0)
 ;;=5^Crohn's Disease
 ;;^UTILITY(U,$J,358.3,12139,2)
 ;;=Crohn's Disease^29356
 ;;^UTILITY(U,$J,358.3,12140,0)
 ;;=530.10^^84^801^15
 ;;^UTILITY(U,$J,358.3,12140,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12140,1,4,0)
 ;;=4^530.10
 ;;^UTILITY(U,$J,358.3,12140,1,5,0)
 ;;=5^Esophagitis
 ;;^UTILITY(U,$J,358.3,12140,2)
 ;;=^295809
 ;;^UTILITY(U,$J,358.3,12141,0)
 ;;=571.8^^84^801^16
 ;;^UTILITY(U,$J,358.3,12141,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12141,1,4,0)
 ;;=4^571.8
 ;;^UTILITY(U,$J,358.3,12141,1,5,0)
 ;;=5^Fatty Liver 
 ;;^UTILITY(U,$J,358.3,12141,2)
 ;;=^87404
 ;;^UTILITY(U,$J,358.3,12142,0)
 ;;=792.1^^84^801^21
 ;;^UTILITY(U,$J,358.3,12142,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12142,1,4,0)
 ;;=4^792.1
 ;;^UTILITY(U,$J,358.3,12142,1,5,0)
 ;;=5^Heme+Stool
 ;;^UTILITY(U,$J,358.3,12142,2)
 ;;=^273412
 ;;^UTILITY(U,$J,358.3,12143,0)
 ;;=578.9^^84^801^17
 ;;^UTILITY(U,$J,358.3,12143,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12143,1,4,0)
 ;;=4^578.9
 ;;^UTILITY(U,$J,358.3,12143,1,5,0)
 ;;=5^GI Bleed
 ;;^UTILITY(U,$J,358.3,12143,2)
 ;;=GI Bleed^49525
 ;;^UTILITY(U,$J,358.3,12144,0)
 ;;=535.50^^84^801^20
 ;;^UTILITY(U,$J,358.3,12144,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12144,1,4,0)
 ;;=4^535.50
 ;;^UTILITY(U,$J,358.3,12144,1,5,0)
 ;;=5^Gastritis
 ;;^UTILITY(U,$J,358.3,12144,2)
 ;;=^270181
 ;;^UTILITY(U,$J,358.3,12145,0)
 ;;=789.1^^84^801^23
 ;;^UTILITY(U,$J,358.3,12145,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12145,1,4,0)
 ;;=4^789.1
 ;;^UTILITY(U,$J,358.3,12145,1,5,0)
 ;;=5^Hepatomegaly
 ;;^UTILITY(U,$J,358.3,12145,2)
 ;;=^56494
 ;;^UTILITY(U,$J,358.3,12146,0)
 ;;=577.1^^84^801^27
 ;;^UTILITY(U,$J,358.3,12146,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12146,1,4,0)
 ;;=4^577.1
 ;;^UTILITY(U,$J,358.3,12146,1,5,0)
 ;;=5^Pancreatitis
 ;;^UTILITY(U,$J,358.3,12146,2)
 ;;=^259100
 ;;^UTILITY(U,$J,358.3,12147,0)
 ;;=789.2^^84^801^30
 ;;^UTILITY(U,$J,358.3,12147,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12147,1,4,0)
 ;;=4^789.2
 ;;^UTILITY(U,$J,358.3,12147,1,5,0)
 ;;=5^Splenomegaly
 ;;^UTILITY(U,$J,358.3,12147,2)
 ;;=^113452
 ;;^UTILITY(U,$J,358.3,12148,0)
 ;;=564.00^^84^801^9
 ;;^UTILITY(U,$J,358.3,12148,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12148,1,4,0)
 ;;=4^564.00
 ;;^UTILITY(U,$J,358.3,12148,1,5,0)
 ;;=5^Constipation
 ;;^UTILITY(U,$J,358.3,12148,2)
 ;;=Constipation^323537
 ;;^UTILITY(U,$J,358.3,12149,0)
 ;;=789.00^^84^801^2
 ;;^UTILITY(U,$J,358.3,12149,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12149,1,4,0)
 ;;=4^789.00
 ;;^UTILITY(U,$J,358.3,12149,1,5,0)
 ;;=5^Abdominal Pain
 ;;^UTILITY(U,$J,358.3,12149,2)
 ;;=^303317
 ;;^UTILITY(U,$J,358.3,12150,0)
 ;;=789.60^^84^801^3
 ;;^UTILITY(U,$J,358.3,12150,1,0)
 ;;=^358.31IA^5^2
