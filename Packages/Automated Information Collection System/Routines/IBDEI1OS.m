IBDEI1OS ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,28665,1,3,0)
 ;;=3^NEEDLE PLACEMENT FOR RT,PELVIC
 ;;^UTILITY(U,$J,358.3,28666,0)
 ;;=J3420^^113^1434^1^^^^1
 ;;^UTILITY(U,$J,358.3,28666,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28666,1,2,0)
 ;;=2^J3420
 ;;^UTILITY(U,$J,358.3,28666,1,3,0)
 ;;=3^B12 INJ UP TO 1,000MCG
 ;;^UTILITY(U,$J,358.3,28667,0)
 ;;=J0800^^113^1434^3^^^^1
 ;;^UTILITY(U,$J,358.3,28667,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28667,1,2,0)
 ;;=2^J0800
 ;;^UTILITY(U,$J,358.3,28667,1,3,0)
 ;;=3^CORTISONE INJ UP TO 40U
 ;;^UTILITY(U,$J,358.3,28668,0)
 ;;=J9202^^113^1434^19^^^^1
 ;;^UTILITY(U,$J,358.3,28668,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28668,1,2,0)
 ;;=2^J9202
 ;;^UTILITY(U,$J,358.3,28668,1,3,0)
 ;;=3^ZOLADEX 3.6MG
 ;;^UTILITY(U,$J,358.3,28669,0)
 ;;=J1642^^113^1434^8^^^^1
 ;;^UTILITY(U,$J,358.3,28669,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28669,1,2,0)
 ;;=2^J1642
 ;;^UTILITY(U,$J,358.3,28669,1,3,0)
 ;;=3^HEP LOCK FLUSH PER 10U
 ;;^UTILITY(U,$J,358.3,28670,0)
 ;;=J7307^^113^1434^4^^^^1
 ;;^UTILITY(U,$J,358.3,28670,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28670,1,2,0)
 ;;=2^J7307
 ;;^UTILITY(U,$J,358.3,28670,1,3,0)
 ;;=3^ETONOGESTREL IMPLANT W/ SUPPLIES
 ;;^UTILITY(U,$J,358.3,28671,0)
 ;;=J1950^^113^1434^14^^^^1
 ;;^UTILITY(U,$J,358.3,28671,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28671,1,2,0)
 ;;=2^J1950
 ;;^UTILITY(U,$J,358.3,28671,1,3,0)
 ;;=3^LEUPROLIDE ACETATE 3.75MG
 ;;^UTILITY(U,$J,358.3,28672,0)
 ;;=J1050^^113^1434^16^^^^1
 ;;^UTILITY(U,$J,358.3,28672,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28672,1,2,0)
 ;;=2^J1050
 ;;^UTILITY(U,$J,358.3,28672,1,3,0)
 ;;=3^MEDROXYPROGESTERONE 1MG
 ;;^UTILITY(U,$J,358.3,28673,0)
 ;;=90649^^113^1434^10^^^^1
 ;;^UTILITY(U,$J,358.3,28673,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28673,1,2,0)
 ;;=2^90649
 ;;^UTILITY(U,$J,358.3,28673,1,3,0)
 ;;=3^HPV VACCINE 3 DOSE
 ;;^UTILITY(U,$J,358.3,28674,0)
 ;;=90656^^113^1434^7^^^^1
 ;;^UTILITY(U,$J,358.3,28674,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28674,1,2,0)
 ;;=2^90656
 ;;^UTILITY(U,$J,358.3,28674,1,3,0)
 ;;=3^FLU,TRIVALENT,SPLIT,NO PRESERVATIVES
 ;;^UTILITY(U,$J,358.3,28675,0)
 ;;=90658^^113^1434^6^^^^1
 ;;^UTILITY(U,$J,358.3,28675,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28675,1,2,0)
 ;;=2^90658
 ;;^UTILITY(U,$J,358.3,28675,1,3,0)
 ;;=3^FLU,TRIVALENT,SPLIT VACCINE
 ;;^UTILITY(U,$J,358.3,28676,0)
 ;;=90732^^113^1434^17^^^^1
 ;;^UTILITY(U,$J,358.3,28676,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28676,1,2,0)
 ;;=2^90732
 ;;^UTILITY(U,$J,358.3,28676,1,3,0)
 ;;=3^PNEUMOCOCCAL VACCINE
 ;;^UTILITY(U,$J,358.3,28677,0)
 ;;=90715^^113^1434^18^^^^1
 ;;^UTILITY(U,$J,358.3,28677,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28677,1,2,0)
 ;;=2^90715
 ;;^UTILITY(U,$J,358.3,28677,1,3,0)
 ;;=3^TDAP VACCINE
 ;;^UTILITY(U,$J,358.3,28678,0)
 ;;=90686^^113^1434^5^^^^1
 ;;^UTILITY(U,$J,358.3,28678,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28678,1,2,0)
 ;;=2^90686
 ;;^UTILITY(U,$J,358.3,28678,1,3,0)
 ;;=3^FLU,QUADRIVALENT,PRESERV-FREE VACCINE
 ;;^UTILITY(U,$J,358.3,28679,0)
 ;;=J0696^^113^1434^2^^^^1
 ;;^UTILITY(U,$J,358.3,28679,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28679,1,2,0)
 ;;=2^J0696
 ;;^UTILITY(U,$J,358.3,28679,1,3,0)
 ;;=3^CEFTRIAXONE SODIUM 250MG
 ;;^UTILITY(U,$J,358.3,28680,0)
 ;;=90746^^113^1434^9^^^^1
 ;;^UTILITY(U,$J,358.3,28680,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,28680,1,2,0)
 ;;=2^90746
 ;;^UTILITY(U,$J,358.3,28680,1,3,0)
 ;;=3^HEPATITIS B VACCINE
 ;;^UTILITY(U,$J,358.3,28681,0)
 ;;=J7300^^113^1434^11^^^^1
 ;;^UTILITY(U,$J,358.3,28681,1,0)
 ;;=^358.31IA^3^2
