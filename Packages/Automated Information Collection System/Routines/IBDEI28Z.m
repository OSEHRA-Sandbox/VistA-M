IBDEI28Z ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,38128,1,4,0)
 ;;=4^R41.9
 ;;^UTILITY(U,$J,358.3,38128,2)
 ;;=^5019449
 ;;^UTILITY(U,$J,358.3,38129,0)
 ;;=F29.^^145^1841^7
 ;;^UTILITY(U,$J,358.3,38129,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38129,1,3,0)
 ;;=3^Schizophrenia Spectrum/Psychotic Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,38129,1,4,0)
 ;;=4^F29.
 ;;^UTILITY(U,$J,358.3,38129,2)
 ;;=^5003484
 ;;^UTILITY(U,$J,358.3,38130,0)
 ;;=F07.0^^145^1841^4
 ;;^UTILITY(U,$J,358.3,38130,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38130,1,3,0)
 ;;=3^Personality Change d/t Another Medical Condition
 ;;^UTILITY(U,$J,358.3,38130,1,4,0)
 ;;=4^F07.0
 ;;^UTILITY(U,$J,358.3,38130,2)
 ;;=^5003063
 ;;^UTILITY(U,$J,358.3,38131,0)
 ;;=Z91.49^^145^1842^12
 ;;^UTILITY(U,$J,358.3,38131,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38131,1,3,0)
 ;;=3^Personal Hx of Psychological Trauma
 ;;^UTILITY(U,$J,358.3,38131,1,4,0)
 ;;=4^Z91.49
 ;;^UTILITY(U,$J,358.3,38131,2)
 ;;=^5063623
 ;;^UTILITY(U,$J,358.3,38132,0)
 ;;=Z91.5^^145^1842^13
 ;;^UTILITY(U,$J,358.3,38132,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38132,1,3,0)
 ;;=3^Personal Hx of Self-Harm
 ;;^UTILITY(U,$J,358.3,38132,1,4,0)
 ;;=4^Z91.5
 ;;^UTILITY(U,$J,358.3,38132,2)
 ;;=^5063624
 ;;^UTILITY(U,$J,358.3,38133,0)
 ;;=Z91.82^^145^1842^11
 ;;^UTILITY(U,$J,358.3,38133,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38133,1,3,0)
 ;;=3^Personal Hx of Military Deployment
 ;;^UTILITY(U,$J,358.3,38133,1,4,0)
 ;;=4^Z91.82
 ;;^UTILITY(U,$J,358.3,38133,2)
 ;;=^5063626
 ;;^UTILITY(U,$J,358.3,38134,0)
 ;;=Z91.89^^145^1842^14
 ;;^UTILITY(U,$J,358.3,38134,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38134,1,3,0)
 ;;=3^Personal Risk Factors
 ;;^UTILITY(U,$J,358.3,38134,1,4,0)
 ;;=4^Z91.89
 ;;^UTILITY(U,$J,358.3,38134,2)
 ;;=^5063628
 ;;^UTILITY(U,$J,358.3,38135,0)
 ;;=Z72.9^^145^1842^15
 ;;^UTILITY(U,$J,358.3,38135,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38135,1,3,0)
 ;;=3^Problem Related to Lifestyle
 ;;^UTILITY(U,$J,358.3,38135,1,4,0)
 ;;=4^Z72.9
 ;;^UTILITY(U,$J,358.3,38135,2)
 ;;=^5063267
 ;;^UTILITY(U,$J,358.3,38136,0)
 ;;=Z72.811^^145^1842^1
 ;;^UTILITY(U,$J,358.3,38136,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38136,1,3,0)
 ;;=3^Adult Antisocial Behavior
 ;;^UTILITY(U,$J,358.3,38136,1,4,0)
 ;;=4^Z72.811
 ;;^UTILITY(U,$J,358.3,38136,2)
 ;;=^5063263
 ;;^UTILITY(U,$J,358.3,38137,0)
 ;;=Z91.19^^145^1842^5
 ;;^UTILITY(U,$J,358.3,38137,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38137,1,3,0)
 ;;=3^Nonadherence to Medical Treatment
 ;;^UTILITY(U,$J,358.3,38137,1,4,0)
 ;;=4^Z91.19
 ;;^UTILITY(U,$J,358.3,38137,2)
 ;;=^5063618
 ;;^UTILITY(U,$J,358.3,38138,0)
 ;;=E66.9^^145^1842^6
 ;;^UTILITY(U,$J,358.3,38138,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38138,1,3,0)
 ;;=3^Obesity
 ;;^UTILITY(U,$J,358.3,38138,1,4,0)
 ;;=4^E66.9
 ;;^UTILITY(U,$J,358.3,38138,2)
 ;;=^5002832
 ;;^UTILITY(U,$J,358.3,38139,0)
 ;;=Z76.5^^145^1842^3
 ;;^UTILITY(U,$J,358.3,38139,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38139,1,3,0)
 ;;=3^Malingering
 ;;^UTILITY(U,$J,358.3,38139,1,4,0)
 ;;=4^Z76.5
 ;;^UTILITY(U,$J,358.3,38139,2)
 ;;=^5063302
 ;;^UTILITY(U,$J,358.3,38140,0)
 ;;=R41.83^^145^1842^2
 ;;^UTILITY(U,$J,358.3,38140,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38140,1,3,0)
 ;;=3^Borderline Intellectual Functioning
 ;;^UTILITY(U,$J,358.3,38140,1,4,0)
 ;;=4^R41.83
 ;;^UTILITY(U,$J,358.3,38140,2)
 ;;=^5019442
 ;;^UTILITY(U,$J,358.3,38141,0)
 ;;=Z56.82^^145^1842^4
 ;;^UTILITY(U,$J,358.3,38141,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38141,1,3,0)
 ;;=3^Military Deployment Status,Current
 ;;^UTILITY(U,$J,358.3,38141,1,4,0)
 ;;=4^Z56.82
