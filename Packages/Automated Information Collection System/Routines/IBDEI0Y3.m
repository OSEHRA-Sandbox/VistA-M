IBDEI0Y3 ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,34310,1,4,0)
 ;;=4^J91.0
 ;;^UTILITY(U,$J,358.3,34310,2)
 ;;=^336603
 ;;^UTILITY(U,$J,358.3,34311,0)
 ;;=J90.^^125^1629^67
 ;;^UTILITY(U,$J,358.3,34311,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34311,1,3,0)
 ;;=3^Pleural Effusion NEC
 ;;^UTILITY(U,$J,358.3,34311,1,4,0)
 ;;=4^J90.
 ;;^UTILITY(U,$J,358.3,34311,2)
 ;;=^5008310
 ;;^UTILITY(U,$J,358.3,34312,0)
 ;;=I10.^^125^1629^37
 ;;^UTILITY(U,$J,358.3,34312,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34312,1,3,0)
 ;;=3^Hypertension,Essential
 ;;^UTILITY(U,$J,358.3,34312,1,4,0)
 ;;=4^I10.
 ;;^UTILITY(U,$J,358.3,34312,2)
 ;;=^5007062
 ;;^UTILITY(U,$J,358.3,34313,0)
 ;;=R06.89^^125^1629^17
 ;;^UTILITY(U,$J,358.3,34313,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34313,1,3,0)
 ;;=3^Breathing Abnormalities
 ;;^UTILITY(U,$J,358.3,34313,1,4,0)
 ;;=4^R06.89
 ;;^UTILITY(U,$J,358.3,34313,2)
 ;;=^5019193
 ;;^UTILITY(U,$J,358.3,34314,0)
 ;;=E86.1^^125^1629^38
 ;;^UTILITY(U,$J,358.3,34314,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34314,1,3,0)
 ;;=3^Hypovolemia
 ;;^UTILITY(U,$J,358.3,34314,1,4,0)
 ;;=4^E86.1
 ;;^UTILITY(U,$J,358.3,34314,2)
 ;;=^332744
 ;;^UTILITY(U,$J,358.3,34315,0)
 ;;=C34.11^^125^1629^51
 ;;^UTILITY(U,$J,358.3,34315,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34315,1,3,0)
 ;;=3^Malig Neop Upper Lobe,Rt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34315,1,4,0)
 ;;=4^C34.11
 ;;^UTILITY(U,$J,358.3,34315,2)
 ;;=^5000961
 ;;^UTILITY(U,$J,358.3,34316,0)
 ;;=C34.12^^125^1629^50
 ;;^UTILITY(U,$J,358.3,34316,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34316,1,3,0)
 ;;=3^Malig Neop Upper Lobe,Lt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34316,1,4,0)
 ;;=4^C34.12
 ;;^UTILITY(U,$J,358.3,34316,2)
 ;;=^5000962
 ;;^UTILITY(U,$J,358.3,34317,0)
 ;;=C34.2^^125^1629^44
 ;;^UTILITY(U,$J,358.3,34317,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34317,1,3,0)
 ;;=3^Malig Neop Middle Lobe,Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34317,1,4,0)
 ;;=4^C34.2
 ;;^UTILITY(U,$J,358.3,34317,2)
 ;;=^267137
 ;;^UTILITY(U,$J,358.3,34318,0)
 ;;=C34.31^^125^1629^42
 ;;^UTILITY(U,$J,358.3,34318,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34318,1,3,0)
 ;;=3^Malig Neop Lower Lobe,Rt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34318,1,4,0)
 ;;=4^C34.31
 ;;^UTILITY(U,$J,358.3,34318,2)
 ;;=^5133321
 ;;^UTILITY(U,$J,358.3,34319,0)
 ;;=C34.32^^125^1629^41
 ;;^UTILITY(U,$J,358.3,34319,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34319,1,3,0)
 ;;=3^Malig Neop Lower Lobe,Lt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34319,1,4,0)
 ;;=4^C34.32
 ;;^UTILITY(U,$J,358.3,34319,2)
 ;;=^5133322
 ;;^UTILITY(U,$J,358.3,34320,0)
 ;;=C34.81^^125^1629^46
 ;;^UTILITY(U,$J,358.3,34320,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34320,1,3,0)
 ;;=3^Malig Neop Overlapping Sites,Rt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34320,1,4,0)
 ;;=4^C34.81
 ;;^UTILITY(U,$J,358.3,34320,2)
 ;;=^5000964
 ;;^UTILITY(U,$J,358.3,34321,0)
 ;;=C34.82^^125^1629^45
 ;;^UTILITY(U,$J,358.3,34321,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34321,1,3,0)
 ;;=3^Malig Neop Overlapping Sites,Lt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34321,1,4,0)
 ;;=4^C34.82
 ;;^UTILITY(U,$J,358.3,34321,2)
 ;;=^5000965
 ;;^UTILITY(U,$J,358.3,34322,0)
 ;;=C34.91^^125^1629^49
 ;;^UTILITY(U,$J,358.3,34322,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34322,1,3,0)
 ;;=3^Malig Neop Unspec Part,Rt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34322,1,4,0)
 ;;=4^C34.91
 ;;^UTILITY(U,$J,358.3,34322,2)
 ;;=^5000967
 ;;^UTILITY(U,$J,358.3,34323,0)
 ;;=C34.92^^125^1629^48
 ;;^UTILITY(U,$J,358.3,34323,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34323,1,3,0)
 ;;=3^Malig Neop Unspec Part,Lt Bronchus/Lung
 ;;^UTILITY(U,$J,358.3,34323,1,4,0)
 ;;=4^C34.92
 ;;^UTILITY(U,$J,358.3,34323,2)
 ;;=^5000968
 ;;^UTILITY(U,$J,358.3,34324,0)
 ;;=C38.4^^125^1629^47
 ;;^UTILITY(U,$J,358.3,34324,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34324,1,3,0)
 ;;=3^Malig Neop Pleura
 ;;^UTILITY(U,$J,358.3,34324,1,4,0)
 ;;=4^C38.4
 ;;^UTILITY(U,$J,358.3,34324,2)
 ;;=^267140
 ;;^UTILITY(U,$J,358.3,34325,0)
 ;;=C39.9^^125^1629^43
 ;;^UTILITY(U,$J,358.3,34325,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34325,1,3,0)
 ;;=3^Malig Neop Lower Respiratory Tract
 ;;^UTILITY(U,$J,358.3,34325,1,4,0)
 ;;=4^C39.9
 ;;^UTILITY(U,$J,358.3,34325,2)
 ;;=^5000972
 ;;^UTILITY(U,$J,358.3,34326,0)
 ;;=R76.11^^125^1629^87
 ;;^UTILITY(U,$J,358.3,34326,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34326,1,3,0)
 ;;=3^Reaction to Skin Test w/o Active TB
 ;;^UTILITY(U,$J,358.3,34326,1,4,0)
 ;;=4^R76.11
 ;;^UTILITY(U,$J,358.3,34326,2)
 ;;=^5019570
 ;;^UTILITY(U,$J,358.3,34327,0)
 ;;=E66.9^^125^1629^65
 ;;^UTILITY(U,$J,358.3,34327,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34327,1,3,0)
 ;;=3^Obesity,Unspec
 ;;^UTILITY(U,$J,358.3,34327,1,4,0)
 ;;=4^E66.9
 ;;^UTILITY(U,$J,358.3,34327,2)
 ;;=^5002832
 ;;^UTILITY(U,$J,358.3,34328,0)
 ;;=I26.99^^125^1629^79
 ;;^UTILITY(U,$J,358.3,34328,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34328,1,3,0)
 ;;=3^Pulmonary Embolism w/o Acute Cor Pulmonale
 ;;^UTILITY(U,$J,358.3,34328,1,4,0)
 ;;=4^I26.99
 ;;^UTILITY(U,$J,358.3,34328,2)
 ;;=^5007150
 ;;^UTILITY(U,$J,358.3,34329,0)
 ;;=R09.1^^125^1629^69
 ;;^UTILITY(U,$J,358.3,34329,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34329,1,3,0)
 ;;=3^Pleurisy
 ;;^UTILITY(U,$J,358.3,34329,1,4,0)
 ;;=4^R09.1
 ;;^UTILITY(U,$J,358.3,34329,2)
 ;;=^95428
 ;;^UTILITY(U,$J,358.3,34330,0)
 ;;=J69.0^^125^1629^75
 ;;^UTILITY(U,$J,358.3,34330,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34330,1,3,0)
 ;;=3^Pneumonitis d/t Inhalation of Food/Vomit
 ;;^UTILITY(U,$J,358.3,34330,1,4,0)
 ;;=4^J69.0
 ;;^UTILITY(U,$J,358.3,34330,2)
 ;;=^5008288
 ;;^UTILITY(U,$J,358.3,34331,0)
 ;;=J15.9^^125^1629^71
 ;;^UTILITY(U,$J,358.3,34331,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34331,1,3,0)
 ;;=3^Pneumonia,Bacterial,Unspec
 ;;^UTILITY(U,$J,358.3,34331,1,4,0)
 ;;=4^J15.9
 ;;^UTILITY(U,$J,358.3,34331,2)
 ;;=^5008178
 ;;^UTILITY(U,$J,358.3,34332,0)
 ;;=J18.9^^125^1629^73
 ;;^UTILITY(U,$J,358.3,34332,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34332,1,3,0)
 ;;=3^Pneumonia,Unspec Organism
 ;;^UTILITY(U,$J,358.3,34332,1,4,0)
 ;;=4^J18.9
 ;;^UTILITY(U,$J,358.3,34332,2)
 ;;=^95632
 ;;^UTILITY(U,$J,358.3,34333,0)
 ;;=J12.9^^125^1629^74
 ;;^UTILITY(U,$J,358.3,34333,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34333,1,3,0)
 ;;=3^Pneumonia,Viral,Unspec
 ;;^UTILITY(U,$J,358.3,34333,1,4,0)
 ;;=4^J12.9
 ;;^UTILITY(U,$J,358.3,34333,2)
 ;;=^5008169
 ;;^UTILITY(U,$J,358.3,34334,0)
 ;;=J93.9^^125^1629^76
 ;;^UTILITY(U,$J,358.3,34334,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34334,1,3,0)
 ;;=3^Pneumothorax,Unspec
 ;;^UTILITY(U,$J,358.3,34334,1,4,0)
 ;;=4^J93.9
 ;;^UTILITY(U,$J,358.3,34334,2)
 ;;=^5008315
 ;;^UTILITY(U,$J,358.3,34335,0)
 ;;=J84.10^^125^1629^82
 ;;^UTILITY(U,$J,358.3,34335,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34335,1,3,0)
 ;;=3^Pulmonary Fibrosis,Unspec
 ;;^UTILITY(U,$J,358.3,34335,1,4,0)
 ;;=4^J84.10
 ;;^UTILITY(U,$J,358.3,34335,2)
 ;;=^5008300
 ;;^UTILITY(U,$J,358.3,34336,0)
 ;;=J84.89^^125^1629^39
 ;;^UTILITY(U,$J,358.3,34336,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34336,1,3,0)
 ;;=3^Interstitial Pulmonary Disease
 ;;^UTILITY(U,$J,358.3,34336,1,4,0)
 ;;=4^J84.89
 ;;^UTILITY(U,$J,358.3,34336,2)
 ;;=^5008303
 ;;^UTILITY(U,$J,358.3,34337,0)
 ;;=Z01.811^^125^1629^77
 ;;^UTILITY(U,$J,358.3,34337,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34337,1,3,0)
 ;;=3^Pre-Procedural Respiratory Exam
 ;;^UTILITY(U,$J,358.3,34337,1,4,0)
 ;;=4^Z01.811
 ;;^UTILITY(U,$J,358.3,34337,2)
 ;;=^5062626
 ;;^UTILITY(U,$J,358.3,34338,0)
 ;;=J18.2^^125^1629^72
 ;;^UTILITY(U,$J,358.3,34338,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,34338,1,3,0)
 ;;=3^Pneumonia,Hypostatic,Unspec Organism
 ;;^UTILITY(U,$J,358.3,34338,1,4,0)
 ;;=4^J18.2
 ;;^UTILITY(U,$J,358.3,34338,2)
 ;;=^5008184
