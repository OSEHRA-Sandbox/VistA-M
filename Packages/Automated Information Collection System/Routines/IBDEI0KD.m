IBDEI0KD ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,9981,1,4,0)
 ;;=4^590.10
 ;;^UTILITY(U,$J,358.3,9981,1,5,0)
 ;;=5^Pyelonephritis, Acute
 ;;^UTILITY(U,$J,358.3,9981,2)
 ;;=^270369
 ;;^UTILITY(U,$J,358.3,9982,0)
 ;;=590.00^^67^670^47
 ;;^UTILITY(U,$J,358.3,9982,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9982,1,4,0)
 ;;=4^590.00
 ;;^UTILITY(U,$J,358.3,9982,1,5,0)
 ;;=5^Pyelonephritis, Chronic
 ;;^UTILITY(U,$J,358.3,9982,2)
 ;;=^270367
 ;;^UTILITY(U,$J,358.3,9983,0)
 ;;=788.7^^67^670^54
 ;;^UTILITY(U,$J,358.3,9983,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9983,1,4,0)
 ;;=4^788.7
 ;;^UTILITY(U,$J,358.3,9983,1,5,0)
 ;;=5^Urethral Discharge
 ;;^UTILITY(U,$J,358.3,9983,2)
 ;;=^265872
 ;;^UTILITY(U,$J,358.3,9984,0)
 ;;=131.02^^67^670^57
 ;;^UTILITY(U,$J,358.3,9984,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9984,1,4,0)
 ;;=4^131.02
 ;;^UTILITY(U,$J,358.3,9984,1,5,0)
 ;;=5^Urethritis, Trichomonal
 ;;^UTILITY(U,$J,358.3,9984,2)
 ;;=^266955
 ;;^UTILITY(U,$J,358.3,9985,0)
 ;;=597.80^^67^670^55
 ;;^UTILITY(U,$J,358.3,9985,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9985,1,4,0)
 ;;=4^597.80
 ;;^UTILITY(U,$J,358.3,9985,1,5,0)
 ;;=5^Urethritis NOS
 ;;^UTILITY(U,$J,358.3,9985,2)
 ;;=Urethritis, NOS^124214
 ;;^UTILITY(U,$J,358.3,9986,0)
 ;;=788.41^^67^670^58
 ;;^UTILITY(U,$J,358.3,9986,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9986,1,4,0)
 ;;=4^788.41
 ;;^UTILITY(U,$J,358.3,9986,1,5,0)
 ;;=5^Urinary Frequency
 ;;^UTILITY(U,$J,358.3,9986,2)
 ;;=^124396
 ;;^UTILITY(U,$J,358.3,9987,0)
 ;;=788.31^^67^670^62
 ;;^UTILITY(U,$J,358.3,9987,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9987,1,4,0)
 ;;=4^788.31
 ;;^UTILITY(U,$J,358.3,9987,1,5,0)
 ;;=5^Urinary Incontinence,Urge
 ;;^UTILITY(U,$J,358.3,9987,2)
 ;;=^260046
 ;;^UTILITY(U,$J,358.3,9988,0)
 ;;=788.20^^67^670^63
 ;;^UTILITY(U,$J,358.3,9988,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9988,1,4,0)
 ;;=4^788.20
 ;;^UTILITY(U,$J,358.3,9988,1,5,0)
 ;;=5^Urinary Retention
 ;;^UTILITY(U,$J,358.3,9988,2)
 ;;=^295812
 ;;^UTILITY(U,$J,358.3,9989,0)
 ;;=112.2^^67^670^66
 ;;^UTILITY(U,$J,358.3,9989,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9989,1,4,0)
 ;;=4^112.2
 ;;^UTILITY(U,$J,358.3,9989,1,5,0)
 ;;=5^Urogenital Candidiasis
 ;;^UTILITY(U,$J,358.3,9989,2)
 ;;=^266866
 ;;^UTILITY(U,$J,358.3,9990,0)
 ;;=600.01^^67^670^5
 ;;^UTILITY(U,$J,358.3,9990,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9990,1,4,0)
 ;;=4^600.01
 ;;^UTILITY(U,$J,358.3,9990,1,5,0)
 ;;=5^BPH with Obstruction
 ;;^UTILITY(U,$J,358.3,9990,2)
 ;;=^329933
 ;;^UTILITY(U,$J,358.3,9991,0)
 ;;=600.10^^67^670^40
 ;;^UTILITY(U,$J,358.3,9991,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9991,1,4,0)
 ;;=4^600.10
 ;;^UTILITY(U,$J,358.3,9991,1,5,0)
 ;;=5^Nod Prostate W/O UR Obst
 ;;^UTILITY(U,$J,358.3,9991,2)
 ;;=^329934
 ;;^UTILITY(U,$J,358.3,9992,0)
 ;;=585.9^^67^670^8
 ;;^UTILITY(U,$J,358.3,9992,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9992,1,4,0)
 ;;=4^585.9
 ;;^UTILITY(U,$J,358.3,9992,1,5,0)
 ;;=5^Chronic Renal Failure
 ;;^UTILITY(U,$J,358.3,9992,2)
 ;;=^332812
 ;;^UTILITY(U,$J,358.3,9993,0)
 ;;=788.64^^67^670^59
 ;;^UTILITY(U,$J,358.3,9993,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9993,1,4,0)
 ;;=4^788.64
 ;;^UTILITY(U,$J,358.3,9993,1,5,0)
 ;;=5^Urinary Hesitancy
 ;;^UTILITY(U,$J,358.3,9993,2)
 ;;=^334165
 ;;^UTILITY(U,$J,358.3,9994,0)
 ;;=788.65^^67^670^52
 ;;^UTILITY(U,$J,358.3,9994,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9994,1,4,0)
 ;;=4^788.65
 ;;^UTILITY(U,$J,358.3,9994,1,5,0)
 ;;=5^Straining of Urination
 ;;^UTILITY(U,$J,358.3,9994,2)
 ;;=^334166
 ;;^UTILITY(U,$J,358.3,9995,0)
 ;;=600.00^^67^670^4
 ;;^UTILITY(U,$J,358.3,9995,1,0)
 ;;=^358.31IA^5^2
