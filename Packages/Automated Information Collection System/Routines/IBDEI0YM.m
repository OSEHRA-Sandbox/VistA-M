IBDEI0YM ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,16253,1,4,0)
 ;;=4^Z85.47
 ;;^UTILITY(U,$J,358.3,16253,2)
 ;;=^5063424
 ;;^UTILITY(U,$J,358.3,16254,0)
 ;;=Z85.12^^61^728^32
 ;;^UTILITY(U,$J,358.3,16254,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16254,1,3,0)
 ;;=3^Personal Hx Malig Neop Trachea
 ;;^UTILITY(U,$J,358.3,16254,1,4,0)
 ;;=4^Z85.12
 ;;^UTILITY(U,$J,358.3,16254,2)
 ;;=^5063409
 ;;^UTILITY(U,$J,358.3,16255,0)
 ;;=Z85.42^^61^728^23
 ;;^UTILITY(U,$J,358.3,16255,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16255,1,3,0)
 ;;=3^Personal Hx Malig Neop Parts of Uterus
 ;;^UTILITY(U,$J,358.3,16255,1,4,0)
 ;;=4^Z85.42
 ;;^UTILITY(U,$J,358.3,16255,2)
 ;;=^5063419
 ;;^UTILITY(U,$J,358.3,16256,0)
 ;;=Z79.01^^61^729^16
 ;;^UTILITY(U,$J,358.3,16256,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16256,1,3,0)
 ;;=3^L/T (Current) Use of Anticoagulants
 ;;^UTILITY(U,$J,358.3,16256,1,4,0)
 ;;=4^Z79.01
 ;;^UTILITY(U,$J,358.3,16256,2)
 ;;=^5063330
 ;;^UTILITY(U,$J,358.3,16257,0)
 ;;=Z76.0^^61^729^18
 ;;^UTILITY(U,$J,358.3,16257,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16257,1,3,0)
 ;;=3^Medication Renewal
 ;;^UTILITY(U,$J,358.3,16257,1,4,0)
 ;;=4^Z76.0
 ;;^UTILITY(U,$J,358.3,16257,2)
 ;;=^5063297
 ;;^UTILITY(U,$J,358.3,16258,0)
 ;;=R79.1^^61^729^2
 ;;^UTILITY(U,$J,358.3,16258,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16258,1,3,0)
 ;;=3^Abnormal Coagulation Profile
 ;;^UTILITY(U,$J,358.3,16258,1,4,0)
 ;;=4^R79.1
 ;;^UTILITY(U,$J,358.3,16258,2)
 ;;=^5019591
 ;;^UTILITY(U,$J,358.3,16259,0)
 ;;=Z95.810^^61^729^1
 ;;^UTILITY(U,$J,358.3,16259,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16259,1,3,0)
 ;;=3^AICD
 ;;^UTILITY(U,$J,358.3,16259,1,4,0)
 ;;=4^Z95.810
 ;;^UTILITY(U,$J,358.3,16259,2)
 ;;=^5063674
 ;;^UTILITY(U,$J,358.3,16260,0)
 ;;=I25.3^^61^729^3
 ;;^UTILITY(U,$J,358.3,16260,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16260,1,3,0)
 ;;=3^Aneurysm of Heart
 ;;^UTILITY(U,$J,358.3,16260,1,4,0)
 ;;=4^I25.3
 ;;^UTILITY(U,$J,358.3,16260,2)
 ;;=^5007112
 ;;^UTILITY(U,$J,358.3,16261,0)
 ;;=I35.9^^61^729^4
 ;;^UTILITY(U,$J,358.3,16261,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16261,1,3,0)
 ;;=3^Aortic Valve Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,16261,1,4,0)
 ;;=4^I35.9
 ;;^UTILITY(U,$J,358.3,16261,2)
 ;;=^5007178
 ;;^UTILITY(U,$J,358.3,16262,0)
 ;;=Z95.2^^61^729^26
 ;;^UTILITY(U,$J,358.3,16262,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16262,1,3,0)
 ;;=3^Presence of Prosthetic Heart Valve
 ;;^UTILITY(U,$J,358.3,16262,1,4,0)
 ;;=4^Z95.2
 ;;^UTILITY(U,$J,358.3,16262,2)
 ;;=^5063670
 ;;^UTILITY(U,$J,358.3,16263,0)
 ;;=I74.9^^61^729^5
 ;;^UTILITY(U,$J,358.3,16263,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16263,1,3,0)
 ;;=3^Arterial Embolism/Thrombosis,Unspec
 ;;^UTILITY(U,$J,358.3,16263,1,4,0)
 ;;=4^I74.9
 ;;^UTILITY(U,$J,358.3,16263,2)
 ;;=^5007805
 ;;^UTILITY(U,$J,358.3,16264,0)
 ;;=I48.91^^61^729^6
 ;;^UTILITY(U,$J,358.3,16264,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16264,1,3,0)
 ;;=3^Atrial Fibrillation,Unspec
 ;;^UTILITY(U,$J,358.3,16264,1,4,0)
 ;;=4^I48.91
 ;;^UTILITY(U,$J,358.3,16264,2)
 ;;=^5007229
 ;;^UTILITY(U,$J,358.3,16265,0)
 ;;=I48.92^^61^729^7
 ;;^UTILITY(U,$J,358.3,16265,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16265,1,3,0)
 ;;=3^Atrial Flutter,Unspec
 ;;^UTILITY(U,$J,358.3,16265,1,4,0)
 ;;=4^I48.92
 ;;^UTILITY(U,$J,358.3,16265,2)
 ;;=^5007230
 ;;^UTILITY(U,$J,358.3,16266,0)
 ;;=Z95.0^^61^729^8
 ;;^UTILITY(U,$J,358.3,16266,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16266,1,3,0)
 ;;=3^Cardiac Pacemaker
 ;;^UTILITY(U,$J,358.3,16266,1,4,0)
 ;;=4^Z95.0
 ;;^UTILITY(U,$J,358.3,16266,2)
 ;;=^5063668
 ;;^UTILITY(U,$J,358.3,16267,0)
 ;;=I42.8^^61^729^9
