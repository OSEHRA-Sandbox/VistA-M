IBDEI0T4 ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,14376,1,4,0)
 ;;=4^614.9
 ;;^UTILITY(U,$J,358.3,14376,1,5,0)
 ;;=5^Pelvic Inflammatory Disease
 ;;^UTILITY(U,$J,358.3,14376,2)
 ;;=^3537
 ;;^UTILITY(U,$J,358.3,14377,0)
 ;;=486.^^90^859^52
 ;;^UTILITY(U,$J,358.3,14377,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14377,1,4,0)
 ;;=4^486.
 ;;^UTILITY(U,$J,358.3,14377,1,5,0)
 ;;=5^Pneumonia
 ;;^UTILITY(U,$J,358.3,14377,2)
 ;;=^95632
 ;;^UTILITY(U,$J,358.3,14378,0)
 ;;=136.3^^90^859^51
 ;;^UTILITY(U,$J,358.3,14378,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14378,1,4,0)
 ;;=4^136.3
 ;;^UTILITY(U,$J,358.3,14378,1,5,0)
 ;;=5^Pneumocystosis
 ;;^UTILITY(U,$J,358.3,14378,2)
 ;;=^95635
 ;;^UTILITY(U,$J,358.3,14379,0)
 ;;=601.9^^90^859^56
 ;;^UTILITY(U,$J,358.3,14379,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14379,1,4,0)
 ;;=4^601.9
 ;;^UTILITY(U,$J,358.3,14379,1,5,0)
 ;;=5^Prostatitis NOS
 ;;^UTILITY(U,$J,358.3,14379,2)
 ;;=^99489
 ;;^UTILITY(U,$J,358.3,14380,0)
 ;;=590.80^^90^859^57
 ;;^UTILITY(U,$J,358.3,14380,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14380,1,4,0)
 ;;=4^590.80
 ;;^UTILITY(U,$J,358.3,14380,1,5,0)
 ;;=5^Pyelonephritis NOS
 ;;^UTILITY(U,$J,358.3,14380,2)
 ;;=^101463
 ;;^UTILITY(U,$J,358.3,14381,0)
 ;;=711.00^^90^859^58
 ;;^UTILITY(U,$J,358.3,14381,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14381,1,4,0)
 ;;=4^711.00
 ;;^UTILITY(U,$J,358.3,14381,1,5,0)
 ;;=5^Septic Joint
 ;;^UTILITY(U,$J,358.3,14381,2)
 ;;=^263780
 ;;^UTILITY(U,$J,358.3,14382,0)
 ;;=461.9^^90^859^60
 ;;^UTILITY(U,$J,358.3,14382,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14382,1,4,0)
 ;;=4^461.9
 ;;^UTILITY(U,$J,358.3,14382,1,5,0)
 ;;=5^Sinusitis, Acute
 ;;^UTILITY(U,$J,358.3,14382,2)
 ;;=^259080
 ;;^UTILITY(U,$J,358.3,14383,0)
 ;;=473.9^^90^859^59
 ;;^UTILITY(U,$J,358.3,14383,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14383,1,4,0)
 ;;=4^473.9
 ;;^UTILITY(U,$J,358.3,14383,1,5,0)
 ;;=5^Sinusitis Chronic
 ;;^UTILITY(U,$J,358.3,14383,2)
 ;;=^123985
 ;;^UTILITY(U,$J,358.3,14384,0)
 ;;=091.0^^90^859^65
 ;;^UTILITY(U,$J,358.3,14384,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14384,1,4,0)
 ;;=4^091.0
 ;;^UTILITY(U,$J,358.3,14384,1,5,0)
 ;;=5^Syphilis,Primary Genital
 ;;^UTILITY(U,$J,358.3,14384,2)
 ;;=^50581
 ;;^UTILITY(U,$J,358.3,14385,0)
 ;;=097.0^^90^859^63
 ;;^UTILITY(U,$J,358.3,14385,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14385,1,4,0)
 ;;=4^097.0
 ;;^UTILITY(U,$J,358.3,14385,1,5,0)
 ;;=5^Syphilis,Latent
 ;;^UTILITY(U,$J,358.3,14385,2)
 ;;=^266781
 ;;^UTILITY(U,$J,358.3,14386,0)
 ;;=094.9^^90^859^64
 ;;^UTILITY(U,$J,358.3,14386,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14386,1,4,0)
 ;;=4^094.9
 ;;^UTILITY(U,$J,358.3,14386,1,5,0)
 ;;=5^Syphilis,Neurosyphilis
 ;;^UTILITY(U,$J,358.3,14386,2)
 ;;=^83026
 ;;^UTILITY(U,$J,358.3,14387,0)
 ;;=097.9^^90^859^62
 ;;^UTILITY(U,$J,358.3,14387,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14387,1,4,0)
 ;;=4^097.9
 ;;^UTILITY(U,$J,358.3,14387,1,5,0)
 ;;=5^Syphilis NOS
 ;;^UTILITY(U,$J,358.3,14387,2)
 ;;=^116808
 ;;^UTILITY(U,$J,358.3,14388,0)
 ;;=463.^^90^859^66
 ;;^UTILITY(U,$J,358.3,14388,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14388,1,4,0)
 ;;=4^463.
 ;;^UTILITY(U,$J,358.3,14388,1,5,0)
 ;;=5^Tonsillitis
 ;;^UTILITY(U,$J,358.3,14388,2)
 ;;=^2695
 ;;^UTILITY(U,$J,358.3,14389,0)
 ;;=130.9^^90^859^67
 ;;^UTILITY(U,$J,358.3,14389,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,14389,1,4,0)
 ;;=4^130.9
 ;;^UTILITY(U,$J,358.3,14389,1,5,0)
 ;;=5^Toxoplasmosis NOS
 ;;^UTILITY(U,$J,358.3,14389,2)
 ;;=^120695
 ;;^UTILITY(U,$J,358.3,14390,0)
 ;;=011.90^^90^859^68
 ;;^UTILITY(U,$J,358.3,14390,1,0)
 ;;=^358.31IA^5^2
