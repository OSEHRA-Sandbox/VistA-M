IBDEI1ZR ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,33343,1,4,0)
 ;;=4^F44.81
 ;;^UTILITY(U,$J,358.3,33343,2)
 ;;=^331909
 ;;^UTILITY(U,$J,358.3,33344,0)
 ;;=F44.9^^148^1637^4
 ;;^UTILITY(U,$J,358.3,33344,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33344,1,3,0)
 ;;=3^Dissociative Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,33344,1,4,0)
 ;;=4^F44.9
 ;;^UTILITY(U,$J,358.3,33344,2)
 ;;=^5003584
 ;;^UTILITY(U,$J,358.3,33345,0)
 ;;=F44.0^^148^1637^2
 ;;^UTILITY(U,$J,358.3,33345,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33345,1,3,0)
 ;;=3^Dissociative Amnesia
 ;;^UTILITY(U,$J,358.3,33345,1,4,0)
 ;;=4^F44.0
 ;;^UTILITY(U,$J,358.3,33345,2)
 ;;=^5003577
 ;;^UTILITY(U,$J,358.3,33346,0)
 ;;=F48.1^^148^1637^1
 ;;^UTILITY(U,$J,358.3,33346,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33346,1,3,0)
 ;;=3^Depersonalization/Derealization Disorder
 ;;^UTILITY(U,$J,358.3,33346,1,4,0)
 ;;=4^F48.1
 ;;^UTILITY(U,$J,358.3,33346,2)
 ;;=^5003593
 ;;^UTILITY(U,$J,358.3,33347,0)
 ;;=F44.89^^148^1637^3
 ;;^UTILITY(U,$J,358.3,33347,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33347,1,3,0)
 ;;=3^Dissociative Disorder NEC
 ;;^UTILITY(U,$J,358.3,33347,1,4,0)
 ;;=4^F44.89
 ;;^UTILITY(U,$J,358.3,33347,2)
 ;;=^5003583
 ;;^UTILITY(U,$J,358.3,33348,0)
 ;;=F50.02^^148^1638^1
 ;;^UTILITY(U,$J,358.3,33348,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33348,1,3,0)
 ;;=3^Anorexia Nervosa,Binge-Eating/Purging Type
 ;;^UTILITY(U,$J,358.3,33348,1,4,0)
 ;;=4^F50.02
 ;;^UTILITY(U,$J,358.3,33348,2)
 ;;=^5003599
 ;;^UTILITY(U,$J,358.3,33349,0)
 ;;=F50.01^^148^1638^2
 ;;^UTILITY(U,$J,358.3,33349,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33349,1,3,0)
 ;;=3^Anorexia Nervosa,Restricting Type
 ;;^UTILITY(U,$J,358.3,33349,1,4,0)
 ;;=4^F50.01
 ;;^UTILITY(U,$J,358.3,33349,2)
 ;;=^5003598
 ;;^UTILITY(U,$J,358.3,33350,0)
 ;;=F50.9^^148^1638^7
 ;;^UTILITY(U,$J,358.3,33350,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33350,1,3,0)
 ;;=3^Feeding/Eating Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,33350,1,4,0)
 ;;=4^F50.9
 ;;^UTILITY(U,$J,358.3,33350,2)
 ;;=^5003602
 ;;^UTILITY(U,$J,358.3,33351,0)
 ;;=F50.8^^148^1638^6
 ;;^UTILITY(U,$J,358.3,33351,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33351,1,3,0)
 ;;=3^Feeding/Eating Disorder NEC
 ;;^UTILITY(U,$J,358.3,33351,1,4,0)
 ;;=4^F50.8
 ;;^UTILITY(U,$J,358.3,33351,2)
 ;;=^5003601
 ;;^UTILITY(U,$J,358.3,33352,0)
 ;;=F50.8^^148^1638^3
 ;;^UTILITY(U,$J,358.3,33352,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33352,1,3,0)
 ;;=3^Avoidant/Restrictive Food Intake Disorder
 ;;^UTILITY(U,$J,358.3,33352,1,4,0)
 ;;=4^F50.8
 ;;^UTILITY(U,$J,358.3,33352,2)
 ;;=^5003601
 ;;^UTILITY(U,$J,358.3,33353,0)
 ;;=F50.8^^148^1638^4
 ;;^UTILITY(U,$J,358.3,33353,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33353,1,3,0)
 ;;=3^Binge-Eating Disorder
 ;;^UTILITY(U,$J,358.3,33353,1,4,0)
 ;;=4^F50.8
 ;;^UTILITY(U,$J,358.3,33353,2)
 ;;=^5003601
 ;;^UTILITY(U,$J,358.3,33354,0)
 ;;=F50.2^^148^1638^5
 ;;^UTILITY(U,$J,358.3,33354,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33354,1,3,0)
 ;;=3^Bulimia Nervosa
 ;;^UTILITY(U,$J,358.3,33354,1,4,0)
 ;;=4^F50.2
 ;;^UTILITY(U,$J,358.3,33354,2)
 ;;=^5003600
 ;;^UTILITY(U,$J,358.3,33355,0)
 ;;=Z55.9^^148^1639^1
 ;;^UTILITY(U,$J,358.3,33355,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33355,1,3,0)
 ;;=3^Acedemic/Educational Problem
 ;;^UTILITY(U,$J,358.3,33355,1,4,0)
 ;;=4^Z55.9
 ;;^UTILITY(U,$J,358.3,33355,2)
 ;;=^5063106
 ;;^UTILITY(U,$J,358.3,33356,0)
 ;;=Z56.81^^148^1639^9
 ;;^UTILITY(U,$J,358.3,33356,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,33356,1,3,0)
 ;;=3^Sexual Harassment on the Job
 ;;^UTILITY(U,$J,358.3,33356,1,4,0)
 ;;=4^Z56.81
