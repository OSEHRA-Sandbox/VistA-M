IBDEI0C2 ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,16234,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16234,1,3,0)
 ;;=3^SHOULDER JOINT REPL STAT
 ;;^UTILITY(U,$J,358.3,16234,1,4,0)
 ;;=4^V43.61
 ;;^UTILITY(U,$J,358.3,16234,2)
 ;;=^303410
 ;;^UTILITY(U,$J,358.3,16235,0)
 ;;=V43.62^^104^903^3
 ;;^UTILITY(U,$J,358.3,16235,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16235,1,3,0)
 ;;=3^ELBOW JOINT REPL STAT
 ;;^UTILITY(U,$J,358.3,16235,1,4,0)
 ;;=4^V43.62
 ;;^UTILITY(U,$J,358.3,16235,2)
 ;;=^303411
 ;;^UTILITY(U,$J,358.3,16236,0)
 ;;=V43.63^^104^903^9
 ;;^UTILITY(U,$J,358.3,16236,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16236,1,3,0)
 ;;=3^WRIST JOINT REPL STAT
 ;;^UTILITY(U,$J,358.3,16236,1,4,0)
 ;;=4^V43.63
 ;;^UTILITY(U,$J,358.3,16236,2)
 ;;=^303412
 ;;^UTILITY(U,$J,358.3,16237,0)
 ;;=V43.64^^104^903^4
 ;;^UTILITY(U,$J,358.3,16237,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16237,1,3,0)
 ;;=3^HIP JOINT REPL STAT
 ;;^UTILITY(U,$J,358.3,16237,1,4,0)
 ;;=4^V43.64
 ;;^UTILITY(U,$J,358.3,16237,2)
 ;;=^303413
 ;;^UTILITY(U,$J,358.3,16238,0)
 ;;=V43.65^^104^903^5
 ;;^UTILITY(U,$J,358.3,16238,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16238,1,3,0)
 ;;=3^KNEE JOINT REPL STAT
 ;;^UTILITY(U,$J,358.3,16238,1,4,0)
 ;;=4^V43.65
 ;;^UTILITY(U,$J,358.3,16238,2)
 ;;=^303414
 ;;^UTILITY(U,$J,358.3,16239,0)
 ;;=V43.66^^104^903^2
 ;;^UTILITY(U,$J,358.3,16239,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16239,1,3,0)
 ;;=3^ANKLE JOINT REPL STAT
 ;;^UTILITY(U,$J,358.3,16239,1,4,0)
 ;;=4^V43.66
 ;;^UTILITY(U,$J,358.3,16239,2)
 ;;=^303415
 ;;^UTILITY(U,$J,358.3,16240,0)
 ;;=V43.69^^104^903^6
 ;;^UTILITY(U,$J,358.3,16240,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16240,1,3,0)
 ;;=3^OTHER JOINT REPL STAT
 ;;^UTILITY(U,$J,358.3,16240,1,4,0)
 ;;=4^V43.69
 ;;^UTILITY(U,$J,358.3,16240,2)
 ;;=^303416
 ;;^UTILITY(U,$J,358.3,16241,0)
 ;;=784.0^^104^904^2
 ;;^UTILITY(U,$J,358.3,16241,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16241,1,3,0)
 ;;=3^HEADACHE
 ;;^UTILITY(U,$J,358.3,16241,1,4,0)
 ;;=4^784.0
 ;;^UTILITY(U,$J,358.3,16241,2)
 ;;=^54133
 ;;^UTILITY(U,$J,358.3,16242,0)
 ;;=346.90^^104^904^8
 ;;^UTILITY(U,$J,358.3,16242,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16242,1,3,0)
 ;;=3^MIGRAINE UNSPEC W/O INTRACT.
 ;;^UTILITY(U,$J,358.3,16242,1,4,0)
 ;;=4^346.90
 ;;^UTILITY(U,$J,358.3,16242,2)
 ;;=^336879
 ;;^UTILITY(U,$J,358.3,16243,0)
 ;;=307.81^^104^904^15
 ;;^UTILITY(U,$J,358.3,16243,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16243,1,3,0)
 ;;=3^TENSION HEADACHE
 ;;^UTILITY(U,$J,358.3,16243,1,4,0)
 ;;=4^307.81
 ;;^UTILITY(U,$J,358.3,16243,2)
 ;;=^100405
 ;;^UTILITY(U,$J,358.3,16244,0)
 ;;=V15.59^^104^904^3
 ;;^UTILITY(U,$J,358.3,16244,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16244,1,3,0)
 ;;=3^HX INJURY NEC
 ;;^UTILITY(U,$J,358.3,16244,1,4,0)
 ;;=4^V15.59
 ;;^UTILITY(U,$J,358.3,16244,2)
 ;;=^336787
 ;;^UTILITY(U,$J,358.3,16245,0)
 ;;=V12.54^^104^904^4
 ;;^UTILITY(U,$J,358.3,16245,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16245,1,3,0)
 ;;=3^HX TIA/STROKE W/O RESID
 ;;^UTILITY(U,$J,358.3,16245,1,4,0)
 ;;=4^V12.54
 ;;^UTILITY(U,$J,358.3,16245,2)
 ;;=^335309
 ;;^UTILITY(U,$J,358.3,16246,0)
 ;;=900.00^^104^904^5
 ;;^UTILITY(U,$J,358.3,16246,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16246,1,3,0)
 ;;=3^INJUR CAROTID ARTERY NOS
 ;;^UTILITY(U,$J,358.3,16246,1,4,0)
 ;;=4^900.00
 ;;^UTILITY(U,$J,358.3,16246,2)
 ;;=^275120
 ;;^UTILITY(U,$J,358.3,16247,0)
 ;;=780.52^^104^904^6
 ;;^UTILITY(U,$J,358.3,16247,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16247,1,3,0)
 ;;=3^INSOMNIA NOS
 ;;^UTILITY(U,$J,358.3,16247,1,4,0)
 ;;=4^780.52
 ;;^UTILITY(U,$J,358.3,16247,2)
 ;;=^332924
 ;;^UTILITY(U,$J,358.3,16248,0)
 ;;=906.5^^104^904^7
 ;;^UTILITY(U,$J,358.3,16248,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16248,1,3,0)
 ;;=3^LATE EFF HEAD/NECK BURN
 ;;^UTILITY(U,$J,358.3,16248,1,4,0)
 ;;=4^906.5
 ;;^UTILITY(U,$J,358.3,16248,2)
 ;;=^275233
 ;;^UTILITY(U,$J,358.3,16249,0)
 ;;=278.00^^104^904^9
 ;;^UTILITY(U,$J,358.3,16249,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16249,1,3,0)
 ;;=3^OBESITY, UNSPEC
 ;;^UTILITY(U,$J,358.3,16249,1,4,0)
 ;;=4^278.00
 ;;^UTILITY(U,$J,358.3,16249,2)
 ;;=^84823
 ;;^UTILITY(U,$J,358.3,16250,0)
 ;;=780.99^^104^904^10
 ;;^UTILITY(U,$J,358.3,16250,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16250,1,3,0)
 ;;=3^OTHER GENERAL SYMPTOMS
 ;;^UTILITY(U,$J,358.3,16250,1,4,0)
 ;;=4^780.99
 ;;^UTILITY(U,$J,358.3,16250,2)
 ;;=^328568
 ;;^UTILITY(U,$J,358.3,16251,0)
 ;;=307.89^^104^904^13
 ;;^UTILITY(U,$J,358.3,16251,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16251,1,3,0)
 ;;=3^PSYCHOGENIC PAIN NEC
 ;;^UTILITY(U,$J,358.3,16251,1,4,0)
 ;;=4^307.89
 ;;^UTILITY(U,$J,358.3,16251,2)
 ;;=^331947
 ;;^UTILITY(U,$J,358.3,16252,0)
 ;;=958.3^^104^904^12
 ;;^UTILITY(U,$J,358.3,16252,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16252,1,3,0)
 ;;=3^POSTTRAUM WND INFEC NEC
 ;;^UTILITY(U,$J,358.3,16252,1,4,0)
 ;;=4^958.3
 ;;^UTILITY(U,$J,358.3,16252,2)
 ;;=^97141
 ;;^UTILITY(U,$J,358.3,16253,0)
 ;;=V82.89^^104^904^14
 ;;^UTILITY(U,$J,358.3,16253,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16253,1,3,0)
 ;;=3^SPC SCRN OTH SPEC CONDITIONS
 ;;^UTILITY(U,$J,358.3,16253,1,4,0)
 ;;=4^V82.89
 ;;^UTILITY(U,$J,358.3,16253,2)
 ;;=^322099
 ;;^UTILITY(U,$J,358.3,16254,0)
 ;;=524.60^^104^904^16
 ;;^UTILITY(U,$J,358.3,16254,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16254,1,3,0)
 ;;=3^TMJ
 ;;^UTILITY(U,$J,358.3,16254,1,4,0)
 ;;=4^524.60
 ;;^UTILITY(U,$J,358.3,16254,2)
 ;;=^117722
 ;;^UTILITY(U,$J,358.3,16255,0)
 ;;=788.30^^104^904^17
 ;;^UTILITY(U,$J,358.3,16255,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16255,1,3,0)
 ;;=3^URINARY INCONTIN
 ;;^UTILITY(U,$J,358.3,16255,1,4,0)
 ;;=4^788.30
 ;;^UTILITY(U,$J,358.3,16255,2)
 ;;=^124400
 ;;^UTILITY(U,$J,358.3,16256,0)
 ;;=783.0^^104^904^1
 ;;^UTILITY(U,$J,358.3,16256,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16256,1,3,0)
 ;;=3^ANOREXIA
 ;;^UTILITY(U,$J,358.3,16256,1,4,0)
 ;;=4^783.0
 ;;^UTILITY(U,$J,358.3,16256,2)
 ;;=^7939
 ;;^UTILITY(U,$J,358.3,16257,0)
 ;;=780.79^^104^904^11
 ;;^UTILITY(U,$J,358.3,16257,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,16257,1,3,0)
 ;;=3^OTHER MALAISE AND FATIGUE
 ;;^UTILITY(U,$J,358.3,16257,1,4,0)
 ;;=4^780.79
 ;;^UTILITY(U,$J,358.3,16257,2)
 ;;=^73344
 ;;^UTILITY(U,$J,358.3,16258,0)
 ;;=99201^^105^905^1
 ;;^UTILITY(U,$J,358.3,16258,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16258,1,1,0)
 ;;=1^Problem Focus
 ;;^UTILITY(U,$J,358.3,16258,1,2,0)
 ;;=2^99201
 ;;^UTILITY(U,$J,358.3,16259,0)
 ;;=99202^^105^905^2
 ;;^UTILITY(U,$J,358.3,16259,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16259,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,16259,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,16260,0)
 ;;=99203^^105^905^3
 ;;^UTILITY(U,$J,358.3,16260,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16260,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,16260,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,16261,0)
 ;;=99204^^105^905^4
 ;;^UTILITY(U,$J,358.3,16261,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16261,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,16261,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,16262,0)
 ;;=99205^^105^905^5
 ;;^UTILITY(U,$J,358.3,16262,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16262,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,16262,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,16263,0)
 ;;=99212^^105^906^2
 ;;^UTILITY(U,$J,358.3,16263,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16263,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,16263,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,16264,0)
 ;;=99213^^105^906^3
 ;;^UTILITY(U,$J,358.3,16264,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16264,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,16264,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,16265,0)
 ;;=99214^^105^906^4
 ;;^UTILITY(U,$J,358.3,16265,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16265,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,16265,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,16266,0)
 ;;=99215^^105^906^5
 ;;^UTILITY(U,$J,358.3,16266,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16266,1,1,0)
 ;;=1^Comprehensive
 ;;^UTILITY(U,$J,358.3,16266,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,16267,0)
 ;;=99241^^105^907^1
 ;;^UTILITY(U,$J,358.3,16267,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16267,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,16267,1,2,0)
 ;;=2^99241
 ;;^UTILITY(U,$J,358.3,16268,0)
 ;;=99242^^105^907^2
 ;;^UTILITY(U,$J,358.3,16268,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16268,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,16268,1,2,0)
 ;;=2^99242
 ;;^UTILITY(U,$J,358.3,16269,0)
 ;;=99243^^105^907^3
 ;;^UTILITY(U,$J,358.3,16269,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16269,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,16269,1,2,0)
 ;;=2^99243
 ;;^UTILITY(U,$J,358.3,16270,0)
 ;;=99244^^105^907^4
 ;;^UTILITY(U,$J,358.3,16270,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16270,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,16270,1,2,0)
 ;;=2^99244
 ;;^UTILITY(U,$J,358.3,16271,0)
 ;;=99245^^105^907^5
 ;;^UTILITY(U,$J,358.3,16271,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16271,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,16271,1,2,0)
 ;;=2^99245
 ;;^UTILITY(U,$J,358.3,16272,0)
 ;;=99024^^105^908^1
 ;;^UTILITY(U,$J,358.3,16272,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,16272,1,1,0)
 ;;=1^Post-Op Follow-up Visit
 ;;^UTILITY(U,$J,358.3,16272,1,2,0)
 ;;=2^99024
 ;;^UTILITY(U,$J,358.3,16273,0)
 ;;=11950^^106^909^72^^^^1
 ;;^UTILITY(U,$J,358.3,16273,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16273,1,2,0)
 ;;=2^SUBC INJ FILL MAT 1 CC OR LESS
 ;;^UTILITY(U,$J,358.3,16273,1,3,0)
 ;;=3^11950
 ;;^UTILITY(U,$J,358.3,16274,0)
 ;;=11951^^106^909^69^^^^1
 ;;^UTILITY(U,$J,358.3,16274,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,16274,1,2,0)
 ;;=2^SUB INJ FILL MAT 1.1- 5.0CC
