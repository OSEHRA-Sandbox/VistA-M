IBDEI0SZ ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,14307,1,4,0)
 ;;=4^785.6
 ;;^UTILITY(U,$J,358.3,14307,1,5,0)
 ;;=5^Lymphadenopathy
 ;;^UTILITY(U,$J,358.3,14307,2)
 ;;=^72368
 ;;^UTILITY(U,$J,358.3,14308,0)
 ;;=200.20^^90^858^90
 ;;^UTILITY(U,$J,358.3,14308,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14308,1,4,0)
 ;;=4^200.20
 ;;^UTILITY(U,$J,358.3,14308,1,5,0)
 ;;=5^Lymphoma,Burkitt's,Site Unspec
 ;;^UTILITY(U,$J,358.3,14308,2)
 ;;=^17529
 ;;^UTILITY(U,$J,358.3,14309,0)
 ;;=202.00^^90^858^92
 ;;^UTILITY(U,$J,358.3,14309,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14309,1,4,0)
 ;;=4^202.00
 ;;^UTILITY(U,$J,358.3,14309,1,5,0)
 ;;=5^Lymphoma,Low-Grade,Site Unspec
 ;;^UTILITY(U,$J,358.3,14309,2)
 ;;=^72606
 ;;^UTILITY(U,$J,358.3,14310,0)
 ;;=200.10^^90^858^91
 ;;^UTILITY(U,$J,358.3,14310,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14310,1,4,0)
 ;;=4^200.10
 ;;^UTILITY(U,$J,358.3,14310,1,5,0)
 ;;=5^Lymphoma,Int or High Grade,Site Unspec
 ;;^UTILITY(U,$J,358.3,14310,2)
 ;;=^175886
 ;;^UTILITY(U,$J,358.3,14311,0)
 ;;=273.3^^90^858^93
 ;;^UTILITY(U,$J,358.3,14311,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14311,1,4,0)
 ;;=4^273.3
 ;;^UTILITY(U,$J,358.3,14311,1,5,0)
 ;;=5^Macroglobulinemia
 ;;^UTILITY(U,$J,358.3,14311,2)
 ;;=^73013
 ;;^UTILITY(U,$J,358.3,14312,0)
 ;;=203.00^^90^858^103
 ;;^UTILITY(U,$J,358.3,14312,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14312,1,4,0)
 ;;=4^203.00
 ;;^UTILITY(U,$J,358.3,14312,1,5,0)
 ;;=5^Multiple Myeloma w/o Remission
 ;;^UTILITY(U,$J,358.3,14312,2)
 ;;=^267514
 ;;^UTILITY(U,$J,358.3,14313,0)
 ;;=203.01^^90^858^105
 ;;^UTILITY(U,$J,358.3,14313,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14313,1,4,0)
 ;;=4^203.01
 ;;^UTILITY(U,$J,358.3,14313,1,5,0)
 ;;=5^Multiple Myeloma,In Remission
 ;;^UTILITY(U,$J,358.3,14313,2)
 ;;=^267515
 ;;^UTILITY(U,$J,358.3,14314,0)
 ;;=238.6^^90^858^108
 ;;^UTILITY(U,$J,358.3,14314,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14314,1,4,0)
 ;;=4^238.6
 ;;^UTILITY(U,$J,358.3,14314,1,5,0)
 ;;=5^Plasmacytoma NOS
 ;;^UTILITY(U,$J,358.3,14314,2)
 ;;=^81973
 ;;^UTILITY(U,$J,358.3,14315,0)
 ;;=205.00^^90^858^4
 ;;^UTILITY(U,$J,358.3,14315,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14315,1,4,0)
 ;;=4^205.00
 ;;^UTILITY(U,$J,358.3,14315,1,5,0)
 ;;=5^AML w/o Remission
 ;;^UTILITY(U,$J,358.3,14315,2)
 ;;=^267531
 ;;^UTILITY(U,$J,358.3,14316,0)
 ;;=205.01^^90^858^6
 ;;^UTILITY(U,$J,358.3,14316,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14316,1,4,0)
 ;;=4^205.01
 ;;^UTILITY(U,$J,358.3,14316,1,5,0)
 ;;=5^AML,In Remission
 ;;^UTILITY(U,$J,358.3,14316,2)
 ;;=^267532
 ;;^UTILITY(U,$J,358.3,14317,0)
 ;;=205.10^^90^858^19
 ;;^UTILITY(U,$J,358.3,14317,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14317,1,4,0)
 ;;=4^205.10
 ;;^UTILITY(U,$J,358.3,14317,1,5,0)
 ;;=5^CML w/o Remission
 ;;^UTILITY(U,$J,358.3,14317,2)
 ;;=^267533
 ;;^UTILITY(U,$J,358.3,14318,0)
 ;;=205.11^^90^858^21
 ;;^UTILITY(U,$J,358.3,14318,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14318,1,4,0)
 ;;=4^205.11
 ;;^UTILITY(U,$J,358.3,14318,1,5,0)
 ;;=5^CML,In Remission
 ;;^UTILITY(U,$J,358.3,14318,2)
 ;;=^267534
 ;;^UTILITY(U,$J,358.3,14319,0)
 ;;=289.0^^90^858^55
 ;;^UTILITY(U,$J,358.3,14319,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14319,1,4,0)
 ;;=4^289.0
 ;;^UTILITY(U,$J,358.3,14319,1,5,0)
 ;;=5^Erthryocytosis, Secondary
 ;;^UTILITY(U,$J,358.3,14319,2)
 ;;=^186856
 ;;^UTILITY(U,$J,358.3,14320,0)
 ;;=238.4^^90^858^109
 ;;^UTILITY(U,$J,358.3,14320,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14320,1,4,0)
 ;;=4^238.4
 ;;^UTILITY(U,$J,358.3,14320,1,5,0)
 ;;=5^Polycytheria Rubra Vera
 ;;^UTILITY(U,$J,358.3,14320,2)
 ;;=^96105
