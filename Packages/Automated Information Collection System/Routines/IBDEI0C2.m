IBDEI0C2 ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,5480,1,2,0)
 ;;=2^99144
 ;;^UTILITY(U,$J,358.3,5480,1,3,0)
 ;;=3^MOD SEDATION,1ST 30MIN
 ;;^UTILITY(U,$J,358.3,5481,0)
 ;;=99145^^28^356^2^^^^1
 ;;^UTILITY(U,$J,358.3,5481,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5481,1,2,0)
 ;;=2^99145
 ;;^UTILITY(U,$J,358.3,5481,1,3,0)
 ;;=3^MOD SEDATION,EA ADDL 15MIN
 ;;^UTILITY(U,$J,358.3,5482,0)
 ;;=11000^^28^357^5^^^^1
 ;;^UTILITY(U,$J,358.3,5482,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5482,1,2,0)
 ;;=2^11000
 ;;^UTILITY(U,$J,358.3,5482,1,3,0)
 ;;=3^DEBRIDE SKIN UP TO 10%
 ;;^UTILITY(U,$J,358.3,5483,0)
 ;;=11042^^28^357^4^^^^1
 ;;^UTILITY(U,$J,358.3,5483,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5483,1,2,0)
 ;;=2^11042
 ;;^UTILITY(U,$J,358.3,5483,1,3,0)
 ;;=3^DEB SUBQ TISSUE 20 SQ CM/<
 ;;^UTILITY(U,$J,358.3,5484,0)
 ;;=11055^^28^357^18^^^^1
 ;;^UTILITY(U,$J,358.3,5484,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5484,1,2,0)
 ;;=2^11055
 ;;^UTILITY(U,$J,358.3,5484,1,3,0)
 ;;=3^TRIM CORN/CALLOUS,SINGLE
 ;;^UTILITY(U,$J,358.3,5485,0)
 ;;=11056^^28^357^16^^^^1
 ;;^UTILITY(U,$J,358.3,5485,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5485,1,2,0)
 ;;=2^11056
 ;;^UTILITY(U,$J,358.3,5485,1,3,0)
 ;;=3^TRIM CORN/CALLOUS,2-4 LESIONS
 ;;^UTILITY(U,$J,358.3,5486,0)
 ;;=11057^^28^357^17^^^^1
 ;;^UTILITY(U,$J,358.3,5486,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5486,1,2,0)
 ;;=2^11057
 ;;^UTILITY(U,$J,358.3,5486,1,3,0)
 ;;=3^TRIM CORN/CALLOUS,5 OR MORE
 ;;^UTILITY(U,$J,358.3,5487,0)
 ;;=11200^^28^357^12^^^^1
 ;;^UTILITY(U,$J,358.3,5487,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5487,1,2,0)
 ;;=2^11200
 ;;^UTILITY(U,$J,358.3,5487,1,3,0)
 ;;=3^REMOVAL OF SKIN TAGS UP TO 15
 ;;^UTILITY(U,$J,358.3,5488,0)
 ;;=11719^^28^357^19^^^^1
 ;;^UTILITY(U,$J,358.3,5488,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5488,1,2,0)
 ;;=2^11719
 ;;^UTILITY(U,$J,358.3,5488,1,3,0)
 ;;=3^TRIM NAILS (NONDYSTROPHIC)
 ;;^UTILITY(U,$J,358.3,5489,0)
 ;;=11730^^28^357^11^^^^1
 ;;^UTILITY(U,$J,358.3,5489,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5489,1,2,0)
 ;;=2^11730
 ;;^UTILITY(U,$J,358.3,5489,1,3,0)
 ;;=3^REMOVAL OF NAIL PLATE
 ;;^UTILITY(U,$J,358.3,5490,0)
 ;;=11740^^28^357^6^^^^1
 ;;^UTILITY(U,$J,358.3,5490,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5490,1,2,0)
 ;;=2^11740
 ;;^UTILITY(U,$J,358.3,5490,1,3,0)
 ;;=3^DRAIN BLOOD FROM UNDER NAIL
 ;;^UTILITY(U,$J,358.3,5491,0)
 ;;=11750^^28^357^10^^^^1
 ;;^UTILITY(U,$J,358.3,5491,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5491,1,2,0)
 ;;=2^11750
 ;;^UTILITY(U,$J,358.3,5491,1,3,0)
 ;;=3^REMOVAL OF NAIL BED
 ;;^UTILITY(U,$J,358.3,5492,0)
 ;;=11760^^28^357^15^^^^1
 ;;^UTILITY(U,$J,358.3,5492,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5492,1,2,0)
 ;;=2^11760
 ;;^UTILITY(U,$J,358.3,5492,1,3,0)
 ;;=3^REPAIR OF NAIL BED
 ;;^UTILITY(U,$J,358.3,5493,0)
 ;;=10060^^28^357^2^^^^1
 ;;^UTILITY(U,$J,358.3,5493,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5493,1,2,0)
 ;;=2^10060
 ;;^UTILITY(U,$J,358.3,5493,1,3,0)
 ;;=3^ABSCESS I&D SIMP/SINGLE
 ;;^UTILITY(U,$J,358.3,5494,0)
 ;;=10061^^28^357^1^^^^1
 ;;^UTILITY(U,$J,358.3,5494,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5494,1,2,0)
 ;;=2^10061
 ;;^UTILITY(U,$J,358.3,5494,1,3,0)
 ;;=3^ABSCESS I&D COMP/MULTI
 ;;^UTILITY(U,$J,358.3,5495,0)
 ;;=10080^^28^357^8^^^^1
 ;;^UTILITY(U,$J,358.3,5495,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5495,1,2,0)
 ;;=2^10080
 ;;^UTILITY(U,$J,358.3,5495,1,3,0)
 ;;=3^DRAINAGE OF PILONIDAL CYST,SIMPLE
 ;;^UTILITY(U,$J,358.3,5496,0)
 ;;=10120^^28^357^14^^^^1
 ;;^UTILITY(U,$J,358.3,5496,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,5496,1,2,0)
 ;;=2^10120
