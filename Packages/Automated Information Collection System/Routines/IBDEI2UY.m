IBDEI2UY ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,47957,1,4,0)
 ;;=4^C22.4
 ;;^UTILITY(U,$J,358.3,47957,2)
 ;;=^5000937
 ;;^UTILITY(U,$J,358.3,47958,0)
 ;;=C22.7^^209^2366^4
 ;;^UTILITY(U,$J,358.3,47958,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47958,1,3,0)
 ;;=3^Liver Carcinomas NEC
 ;;^UTILITY(U,$J,358.3,47958,1,4,0)
 ;;=4^C22.7
 ;;^UTILITY(U,$J,358.3,47958,2)
 ;;=^5000938
 ;;^UTILITY(U,$J,358.3,47959,0)
 ;;=C22.8^^209^2366^8
 ;;^UTILITY(U,$J,358.3,47959,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47959,1,3,0)
 ;;=3^Malig Neop of Liver,Primary
 ;;^UTILITY(U,$J,358.3,47959,1,4,0)
 ;;=4^C22.8
 ;;^UTILITY(U,$J,358.3,47959,2)
 ;;=^5000939
 ;;^UTILITY(U,$J,358.3,47960,0)
 ;;=C22.9^^209^2366^9
 ;;^UTILITY(U,$J,358.3,47960,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47960,1,3,0)
 ;;=3^Malig Neop of Liver,Unspec
 ;;^UTILITY(U,$J,358.3,47960,1,4,0)
 ;;=4^C22.9
 ;;^UTILITY(U,$J,358.3,47960,2)
 ;;=^267096
 ;;^UTILITY(U,$J,358.3,47961,0)
 ;;=C23.^^209^2366^7
 ;;^UTILITY(U,$J,358.3,47961,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47961,1,3,0)
 ;;=3^Malig Neop of Gallbladder
 ;;^UTILITY(U,$J,358.3,47961,1,4,0)
 ;;=4^C23.
 ;;^UTILITY(U,$J,358.3,47961,2)
 ;;=^267098
 ;;^UTILITY(U,$J,358.3,47962,0)
 ;;=C24.0^^209^2367^3
 ;;^UTILITY(U,$J,358.3,47962,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47962,1,3,0)
 ;;=3^Malig Neop of Extrahepatic Bile Duct
 ;;^UTILITY(U,$J,358.3,47962,1,4,0)
 ;;=4^C24.0
 ;;^UTILITY(U,$J,358.3,47962,2)
 ;;=^5000940
 ;;^UTILITY(U,$J,358.3,47963,0)
 ;;=C24.1^^209^2367^1
 ;;^UTILITY(U,$J,358.3,47963,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47963,1,3,0)
 ;;=3^Malig Neop of Ampulla of Vater
 ;;^UTILITY(U,$J,358.3,47963,1,4,0)
 ;;=4^C24.1
 ;;^UTILITY(U,$J,358.3,47963,2)
 ;;=^267100
 ;;^UTILITY(U,$J,358.3,47964,0)
 ;;=C24.9^^209^2367^2
 ;;^UTILITY(U,$J,358.3,47964,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47964,1,3,0)
 ;;=3^Malig Neop of Biliary Tract,Unspec
 ;;^UTILITY(U,$J,358.3,47964,1,4,0)
 ;;=4^C24.9
 ;;^UTILITY(U,$J,358.3,47964,2)
 ;;=^5000942
 ;;^UTILITY(U,$J,358.3,47965,0)
 ;;=C26.0^^209^2368^2
 ;;^UTILITY(U,$J,358.3,47965,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47965,1,3,0)
 ;;=3^Malig Neop of Intestinal Tract,Unspec Part
 ;;^UTILITY(U,$J,358.3,47965,1,4,0)
 ;;=4^C26.0
 ;;^UTILITY(U,$J,358.3,47965,2)
 ;;=^5000947
 ;;^UTILITY(U,$J,358.3,47966,0)
 ;;=C26.1^^209^2368^3
 ;;^UTILITY(U,$J,358.3,47966,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47966,1,3,0)
 ;;=3^Malig Neop of Spleen
 ;;^UTILITY(U,$J,358.3,47966,1,4,0)
 ;;=4^C26.1
 ;;^UTILITY(U,$J,358.3,47966,2)
 ;;=^267116
 ;;^UTILITY(U,$J,358.3,47967,0)
 ;;=C26.9^^209^2368^1
 ;;^UTILITY(U,$J,358.3,47967,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47967,1,3,0)
 ;;=3^Malig Neop of Digestive System,Ill-Defined Sites
 ;;^UTILITY(U,$J,358.3,47967,1,4,0)
 ;;=4^C26.9
 ;;^UTILITY(U,$J,358.3,47967,2)
 ;;=^5000948
 ;;^UTILITY(U,$J,358.3,47968,0)
 ;;=C30.0^^209^2369^6
 ;;^UTILITY(U,$J,358.3,47968,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47968,1,3,0)
 ;;=3^Malig Neop of Nasal Cavity
 ;;^UTILITY(U,$J,358.3,47968,1,4,0)
 ;;=4^C30.0
 ;;^UTILITY(U,$J,358.3,47968,2)
 ;;=^5000949
 ;;^UTILITY(U,$J,358.3,47969,0)
 ;;=C30.1^^209^2369^5
 ;;^UTILITY(U,$J,358.3,47969,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47969,1,3,0)
 ;;=3^Malig Neop of Middle Ear
 ;;^UTILITY(U,$J,358.3,47969,1,4,0)
 ;;=4^C30.1
 ;;^UTILITY(U,$J,358.3,47969,2)
 ;;=^5000950
 ;;^UTILITY(U,$J,358.3,47970,0)
 ;;=C31.0^^209^2369^4
 ;;^UTILITY(U,$J,358.3,47970,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,47970,1,3,0)
 ;;=3^Malig Neop of Maxillary Sinus
 ;;^UTILITY(U,$J,358.3,47970,1,4,0)
 ;;=4^C31.0
