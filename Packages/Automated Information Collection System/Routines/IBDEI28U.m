IBDEI28U ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,38063,1,4,0)
 ;;=4^F48.1
 ;;^UTILITY(U,$J,358.3,38063,2)
 ;;=^5003593
 ;;^UTILITY(U,$J,358.3,38064,0)
 ;;=F44.89^^145^1834^4
 ;;^UTILITY(U,$J,358.3,38064,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38064,1,3,0)
 ;;=3^Dissociative Disorder NEC
 ;;^UTILITY(U,$J,358.3,38064,1,4,0)
 ;;=4^F44.89
 ;;^UTILITY(U,$J,358.3,38064,2)
 ;;=^5003583
 ;;^UTILITY(U,$J,358.3,38065,0)
 ;;=F44.1^^145^1834^3
 ;;^UTILITY(U,$J,358.3,38065,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38065,1,3,0)
 ;;=3^Dissociative Amnesia w/ Dissociative Fugue
 ;;^UTILITY(U,$J,358.3,38065,1,4,0)
 ;;=4^F44.1
 ;;^UTILITY(U,$J,358.3,38065,2)
 ;;=^331908
 ;;^UTILITY(U,$J,358.3,38066,0)
 ;;=F50.02^^145^1835^1
 ;;^UTILITY(U,$J,358.3,38066,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38066,1,3,0)
 ;;=3^Anorexia Nervosa,Binge-Eating/Purging Type
 ;;^UTILITY(U,$J,358.3,38066,1,4,0)
 ;;=4^F50.02
 ;;^UTILITY(U,$J,358.3,38066,2)
 ;;=^5003599
 ;;^UTILITY(U,$J,358.3,38067,0)
 ;;=F50.01^^145^1835^2
 ;;^UTILITY(U,$J,358.3,38067,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38067,1,3,0)
 ;;=3^Anorexia Nervosa,Restricting Type
 ;;^UTILITY(U,$J,358.3,38067,1,4,0)
 ;;=4^F50.01
 ;;^UTILITY(U,$J,358.3,38067,2)
 ;;=^5003598
 ;;^UTILITY(U,$J,358.3,38068,0)
 ;;=F50.9^^145^1835^7
 ;;^UTILITY(U,$J,358.3,38068,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38068,1,3,0)
 ;;=3^Feeding/Eating Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,38068,1,4,0)
 ;;=4^F50.9
 ;;^UTILITY(U,$J,358.3,38068,2)
 ;;=^5003602
 ;;^UTILITY(U,$J,358.3,38069,0)
 ;;=F50.8^^145^1835^6
 ;;^UTILITY(U,$J,358.3,38069,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38069,1,3,0)
 ;;=3^Feeding/Eating Disorder NEC
 ;;^UTILITY(U,$J,358.3,38069,1,4,0)
 ;;=4^F50.8
 ;;^UTILITY(U,$J,358.3,38069,2)
 ;;=^5003601
 ;;^UTILITY(U,$J,358.3,38070,0)
 ;;=F50.8^^145^1835^3
 ;;^UTILITY(U,$J,358.3,38070,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38070,1,3,0)
 ;;=3^Avoidant/Restrictive Food Intake Disorder
 ;;^UTILITY(U,$J,358.3,38070,1,4,0)
 ;;=4^F50.8
 ;;^UTILITY(U,$J,358.3,38070,2)
 ;;=^5003601
 ;;^UTILITY(U,$J,358.3,38071,0)
 ;;=F50.8^^145^1835^4
 ;;^UTILITY(U,$J,358.3,38071,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38071,1,3,0)
 ;;=3^Binge-Eating Disorder
 ;;^UTILITY(U,$J,358.3,38071,1,4,0)
 ;;=4^F50.8
 ;;^UTILITY(U,$J,358.3,38071,2)
 ;;=^5003601
 ;;^UTILITY(U,$J,358.3,38072,0)
 ;;=F50.2^^145^1835^5
 ;;^UTILITY(U,$J,358.3,38072,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38072,1,3,0)
 ;;=3^Bulimia Nervosa
 ;;^UTILITY(U,$J,358.3,38072,1,4,0)
 ;;=4^F50.2
 ;;^UTILITY(U,$J,358.3,38072,2)
 ;;=^5003600
 ;;^UTILITY(U,$J,358.3,38073,0)
 ;;=F50.8^^145^1835^8
 ;;^UTILITY(U,$J,358.3,38073,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38073,1,3,0)
 ;;=3^Pica,In Adults
 ;;^UTILITY(U,$J,358.3,38073,1,4,0)
 ;;=4^F50.8
 ;;^UTILITY(U,$J,358.3,38073,2)
 ;;=^5003601
 ;;^UTILITY(U,$J,358.3,38074,0)
 ;;=Z55.9^^145^1836^1
 ;;^UTILITY(U,$J,358.3,38074,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38074,1,3,0)
 ;;=3^Acedemic/Educational Problem
 ;;^UTILITY(U,$J,358.3,38074,1,4,0)
 ;;=4^Z55.9
 ;;^UTILITY(U,$J,358.3,38074,2)
 ;;=^5063106
 ;;^UTILITY(U,$J,358.3,38075,0)
 ;;=Z56.81^^145^1836^9
 ;;^UTILITY(U,$J,358.3,38075,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38075,1,3,0)
 ;;=3^Sexual Harassment on the Job
 ;;^UTILITY(U,$J,358.3,38075,1,4,0)
 ;;=4^Z56.81
 ;;^UTILITY(U,$J,358.3,38075,2)
 ;;=^5063114
 ;;^UTILITY(U,$J,358.3,38076,0)
 ;;=Z56.9^^145^1836^8
 ;;^UTILITY(U,$J,358.3,38076,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38076,1,3,0)
 ;;=3^Problems Related to Employment NEC
 ;;^UTILITY(U,$J,358.3,38076,1,4,0)
 ;;=4^Z56.9
