IBDEI08N ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,3492,1,4,0)
 ;;=4^N30.90
 ;;^UTILITY(U,$J,358.3,3492,2)
 ;;=^5015642
 ;;^UTILITY(U,$J,358.3,3493,0)
 ;;=R30.0^^28^255^24
 ;;^UTILITY(U,$J,358.3,3493,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3493,1,3,0)
 ;;=3^Dysuria
 ;;^UTILITY(U,$J,358.3,3493,1,4,0)
 ;;=4^R30.0
 ;;^UTILITY(U,$J,358.3,3493,2)
 ;;=^5019322
 ;;^UTILITY(U,$J,358.3,3494,0)
 ;;=N80.9^^28^255^26
 ;;^UTILITY(U,$J,358.3,3494,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3494,1,3,0)
 ;;=3^Endometriosis,Unspec
 ;;^UTILITY(U,$J,358.3,3494,1,4,0)
 ;;=4^N80.9
 ;;^UTILITY(U,$J,358.3,3494,2)
 ;;=^5015837
 ;;^UTILITY(U,$J,358.3,3495,0)
 ;;=N45.1^^28^255^27
 ;;^UTILITY(U,$J,358.3,3495,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3495,1,3,0)
 ;;=3^Epididymitis
 ;;^UTILITY(U,$J,358.3,3495,1,4,0)
 ;;=4^N45.1
 ;;^UTILITY(U,$J,358.3,3495,2)
 ;;=^41396
 ;;^UTILITY(U,$J,358.3,3496,0)
 ;;=N45.3^^28^255^29
 ;;^UTILITY(U,$J,358.3,3496,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3496,1,3,0)
 ;;=3^Epididymo-orchitis
 ;;^UTILITY(U,$J,358.3,3496,1,4,0)
 ;;=4^N45.3
 ;;^UTILITY(U,$J,358.3,3496,2)
 ;;=^5015707
 ;;^UTILITY(U,$J,358.3,3497,0)
 ;;=N52.9^^28^255^30
 ;;^UTILITY(U,$J,358.3,3497,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3497,1,3,0)
 ;;=3^Erectile Dysfunction,Male,Unspec
 ;;^UTILITY(U,$J,358.3,3497,1,4,0)
 ;;=4^N52.9
 ;;^UTILITY(U,$J,358.3,3497,2)
 ;;=^5015763
 ;;^UTILITY(U,$J,358.3,3498,0)
 ;;=N83.9^^28^255^31
 ;;^UTILITY(U,$J,358.3,3498,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3498,1,3,0)
 ;;=3^Fallopion & Broad Ligament Noninflammatory Disorder of Ovary,Unspec
 ;;^UTILITY(U,$J,358.3,3498,1,4,0)
 ;;=4^N83.9
 ;;^UTILITY(U,$J,358.3,3498,2)
 ;;=^5015865
 ;;^UTILITY(U,$J,358.3,3499,0)
 ;;=N82.9^^28^255^32
 ;;^UTILITY(U,$J,358.3,3499,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3499,1,3,0)
 ;;=3^Fistulae,Female Genital Tract,Unspec
 ;;^UTILITY(U,$J,358.3,3499,1,4,0)
 ;;=4^N82.9
 ;;^UTILITY(U,$J,358.3,3499,2)
 ;;=^5015853
 ;;^UTILITY(U,$J,358.3,3500,0)
 ;;=N49.9^^28^255^34
 ;;^UTILITY(U,$J,358.3,3500,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3500,1,3,0)
 ;;=3^Genital Organ Inflammatory Disorder,Male,Unspec
 ;;^UTILITY(U,$J,358.3,3500,1,4,0)
 ;;=4^N49.9
 ;;^UTILITY(U,$J,358.3,3500,2)
 ;;=^5015749
 ;;^UTILITY(U,$J,358.3,3501,0)
 ;;=N50.9^^28^255^33
 ;;^UTILITY(U,$J,358.3,3501,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3501,1,3,0)
 ;;=3^Genital Organ Disorder,Male,Unspec
 ;;^UTILITY(U,$J,358.3,3501,1,4,0)
 ;;=4^N50.9
 ;;^UTILITY(U,$J,358.3,3501,2)
 ;;=^5015751
 ;;^UTILITY(U,$J,358.3,3502,0)
 ;;=N81.10^^28^255^23
 ;;^UTILITY(U,$J,358.3,3502,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3502,1,3,0)
 ;;=3^Cystocele,Unspec
 ;;^UTILITY(U,$J,358.3,3502,1,4,0)
 ;;=4^N81.10
 ;;^UTILITY(U,$J,358.3,3502,2)
 ;;=^5015838
 ;;^UTILITY(U,$J,358.3,3503,0)
 ;;=R39.9^^28^255^35
 ;;^UTILITY(U,$J,358.3,3503,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3503,1,3,0)
 ;;=3^Genitourinary System,Unspec Signs/Symptoms
 ;;^UTILITY(U,$J,358.3,3503,1,4,0)
 ;;=4^R39.9
 ;;^UTILITY(U,$J,358.3,3503,2)
 ;;=^5019351
 ;;^UTILITY(U,$J,358.3,3504,0)
 ;;=R31.9^^28^255^36
 ;;^UTILITY(U,$J,358.3,3504,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3504,1,3,0)
 ;;=3^Hematuria,Unspec
 ;;^UTILITY(U,$J,358.3,3504,1,4,0)
 ;;=4^R31.9
 ;;^UTILITY(U,$J,358.3,3504,2)
 ;;=^5019328
 ;;^UTILITY(U,$J,358.3,3505,0)
 ;;=R39.11^^28^255^37
 ;;^UTILITY(U,$J,358.3,3505,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,3505,1,3,0)
 ;;=3^Hesitancy of Micturition
 ;;^UTILITY(U,$J,358.3,3505,1,4,0)
 ;;=4^R39.11
 ;;^UTILITY(U,$J,358.3,3505,2)
 ;;=^5019341
 ;;^UTILITY(U,$J,358.3,3506,0)
 ;;=N43.3^^28^255^38
