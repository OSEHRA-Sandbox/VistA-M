IBDEI0F4 ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,15121,1,4,0)
 ;;=4^M80.08XD
 ;;^UTILITY(U,$J,358.3,15121,2)
 ;;=^5013496
 ;;^UTILITY(U,$J,358.3,15122,0)
 ;;=M84.48XA^^61^746^118
 ;;^UTILITY(U,$J,358.3,15122,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15122,1,3,0)
 ;;=3^Pathological Fx,Oth Site,Init Encntr
 ;;^UTILITY(U,$J,358.3,15122,1,4,0)
 ;;=4^M84.48XA
 ;;^UTILITY(U,$J,358.3,15122,2)
 ;;=^5014016
 ;;^UTILITY(U,$J,358.3,15123,0)
 ;;=M84.48XD^^61^746^119
 ;;^UTILITY(U,$J,358.3,15123,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15123,1,3,0)
 ;;=3^Pathological Fx,Oth Site,Subs Encntr
 ;;^UTILITY(U,$J,358.3,15123,1,4,0)
 ;;=4^M84.48XD
 ;;^UTILITY(U,$J,358.3,15123,2)
 ;;=^5014017
 ;;^UTILITY(U,$J,358.3,15124,0)
 ;;=M87.011^^61^746^45
 ;;^UTILITY(U,$J,358.3,15124,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15124,1,3,0)
 ;;=3^Idiopathic Aseptic Necrosis of Right Shoulder
 ;;^UTILITY(U,$J,358.3,15124,1,4,0)
 ;;=4^M87.011
 ;;^UTILITY(U,$J,358.3,15124,2)
 ;;=^5014658
 ;;^UTILITY(U,$J,358.3,15125,0)
 ;;=M87.012^^61^746^42
 ;;^UTILITY(U,$J,358.3,15125,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15125,1,3,0)
 ;;=3^Idiopathic Aseptic Necrosis of Left Shoulder
 ;;^UTILITY(U,$J,358.3,15125,1,4,0)
 ;;=4^M87.012
 ;;^UTILITY(U,$J,358.3,15125,2)
 ;;=^5014659
 ;;^UTILITY(U,$J,358.3,15126,0)
 ;;=M87.050^^61^746^43
 ;;^UTILITY(U,$J,358.3,15126,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15126,1,3,0)
 ;;=3^Idiopathic Aseptic Necrosis of Pelvis
 ;;^UTILITY(U,$J,358.3,15126,1,4,0)
 ;;=4^M87.050
 ;;^UTILITY(U,$J,358.3,15126,2)
 ;;=^5014679
 ;;^UTILITY(U,$J,358.3,15127,0)
 ;;=M87.051^^61^746^44
 ;;^UTILITY(U,$J,358.3,15127,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15127,1,3,0)
 ;;=3^Idiopathic Aseptic Necrosis of Right Femur
 ;;^UTILITY(U,$J,358.3,15127,1,4,0)
 ;;=4^M87.051
 ;;^UTILITY(U,$J,358.3,15127,2)
 ;;=^5014680
 ;;^UTILITY(U,$J,358.3,15128,0)
 ;;=M87.052^^61^746^41
 ;;^UTILITY(U,$J,358.3,15128,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15128,1,3,0)
 ;;=3^Idiopathic Aseptic Necrosis of Left Femur
 ;;^UTILITY(U,$J,358.3,15128,1,4,0)
 ;;=4^M87.052
 ;;^UTILITY(U,$J,358.3,15128,2)
 ;;=^5014681
 ;;^UTILITY(U,$J,358.3,15129,0)
 ;;=M87.111^^61^746^74
 ;;^UTILITY(U,$J,358.3,15129,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15129,1,3,0)
 ;;=3^Osteonecrosis d/t Drugs,Right Shoulder
 ;;^UTILITY(U,$J,358.3,15129,1,4,0)
 ;;=4^M87.111
 ;;^UTILITY(U,$J,358.3,15129,2)
 ;;=^5014701
 ;;^UTILITY(U,$J,358.3,15130,0)
 ;;=M87.112^^61^746^71
 ;;^UTILITY(U,$J,358.3,15130,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15130,1,3,0)
 ;;=3^Osteonecrosis d/t Drugs,Left Shoulder
 ;;^UTILITY(U,$J,358.3,15130,1,4,0)
 ;;=4^M87.112
 ;;^UTILITY(U,$J,358.3,15130,2)
 ;;=^5014702
 ;;^UTILITY(U,$J,358.3,15131,0)
 ;;=M87.150^^61^746^72
 ;;^UTILITY(U,$J,358.3,15131,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15131,1,3,0)
 ;;=3^Osteonecrosis d/t Drugs,Pelvis
 ;;^UTILITY(U,$J,358.3,15131,1,4,0)
 ;;=4^M87.150
 ;;^UTILITY(U,$J,358.3,15131,2)
 ;;=^5014722
 ;;^UTILITY(U,$J,358.3,15132,0)
 ;;=M87.151^^61^746^73
 ;;^UTILITY(U,$J,358.3,15132,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15132,1,3,0)
 ;;=3^Osteonecrosis d/t Drugs,Right Femur
 ;;^UTILITY(U,$J,358.3,15132,1,4,0)
 ;;=4^M87.151
 ;;^UTILITY(U,$J,358.3,15132,2)
 ;;=^5014723
 ;;^UTILITY(U,$J,358.3,15133,0)
 ;;=M87.152^^61^746^70
 ;;^UTILITY(U,$J,358.3,15133,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15133,1,3,0)
 ;;=3^Osteonecrosis d/t Drugs,Left Femur
 ;;^UTILITY(U,$J,358.3,15133,1,4,0)
 ;;=4^M87.152
 ;;^UTILITY(U,$J,358.3,15133,2)
 ;;=^5014724
 ;;^UTILITY(U,$J,358.3,15134,0)
 ;;=M87.180^^61^746^69
 ;;^UTILITY(U,$J,358.3,15134,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15134,1,3,0)
 ;;=3^Osteonecrosis d/t Drugs,Jaw
 ;;^UTILITY(U,$J,358.3,15134,1,4,0)
 ;;=4^M87.180
 ;;^UTILITY(U,$J,358.3,15134,2)
 ;;=^5014741
 ;;^UTILITY(U,$J,358.3,15135,0)
 ;;=M87.311^^61^746^168
 ;;^UTILITY(U,$J,358.3,15135,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15135,1,3,0)
 ;;=3^Secondary Osteonecrosis,Right Shoulder
 ;;^UTILITY(U,$J,358.3,15135,1,4,0)
 ;;=4^M87.311
 ;;^UTILITY(U,$J,358.3,15135,2)
 ;;=^5014788
 ;;^UTILITY(U,$J,358.3,15136,0)
 ;;=M87.312^^61^746^165
 ;;^UTILITY(U,$J,358.3,15136,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15136,1,3,0)
 ;;=3^Secondary Osteonecrosis,Left Shoulder
 ;;^UTILITY(U,$J,358.3,15136,1,4,0)
 ;;=4^M87.312
 ;;^UTILITY(U,$J,358.3,15136,2)
 ;;=^5014789
 ;;^UTILITY(U,$J,358.3,15137,0)
 ;;=M87.350^^61^746^166
 ;;^UTILITY(U,$J,358.3,15137,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15137,1,3,0)
 ;;=3^Secondary Osteonecrosis,Pelvis
 ;;^UTILITY(U,$J,358.3,15137,1,4,0)
 ;;=4^M87.350
 ;;^UTILITY(U,$J,358.3,15137,2)
 ;;=^5014809
 ;;^UTILITY(U,$J,358.3,15138,0)
 ;;=M87.351^^61^746^167
 ;;^UTILITY(U,$J,358.3,15138,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15138,1,3,0)
 ;;=3^Secondary Osteonecrosis,Right Femur
 ;;^UTILITY(U,$J,358.3,15138,1,4,0)
 ;;=4^M87.351
 ;;^UTILITY(U,$J,358.3,15138,2)
 ;;=^5014810
 ;;^UTILITY(U,$J,358.3,15139,0)
 ;;=M87.352^^61^746^164
 ;;^UTILITY(U,$J,358.3,15139,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15139,1,3,0)
 ;;=3^Secondary Osteonecrosis,Left Femur
 ;;^UTILITY(U,$J,358.3,15139,1,4,0)
 ;;=4^M87.352
 ;;^UTILITY(U,$J,358.3,15139,2)
 ;;=^5014811
 ;;^UTILITY(U,$J,358.3,15140,0)
 ;;=M87.811^^61^746^79
 ;;^UTILITY(U,$J,358.3,15140,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15140,1,3,0)
 ;;=3^Osteonecrosis,Right Shoulder
 ;;^UTILITY(U,$J,358.3,15140,1,4,0)
 ;;=4^M87.811
 ;;^UTILITY(U,$J,358.3,15140,2)
 ;;=^5014831
 ;;^UTILITY(U,$J,358.3,15141,0)
 ;;=M87.812^^61^746^76
 ;;^UTILITY(U,$J,358.3,15141,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15141,1,3,0)
 ;;=3^Osteonecrosis,Left Shoulder
 ;;^UTILITY(U,$J,358.3,15141,1,4,0)
 ;;=4^M87.812
 ;;^UTILITY(U,$J,358.3,15141,2)
 ;;=^5014832
 ;;^UTILITY(U,$J,358.3,15142,0)
 ;;=M87.850^^61^746^77
 ;;^UTILITY(U,$J,358.3,15142,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15142,1,3,0)
 ;;=3^Osteonecrosis,Pelvis
 ;;^UTILITY(U,$J,358.3,15142,1,4,0)
 ;;=4^M87.850
 ;;^UTILITY(U,$J,358.3,15142,2)
 ;;=^5014852
 ;;^UTILITY(U,$J,358.3,15143,0)
 ;;=M87.851^^61^746^78
 ;;^UTILITY(U,$J,358.3,15143,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15143,1,3,0)
 ;;=3^Osteonecrosis,Right Femur
 ;;^UTILITY(U,$J,358.3,15143,1,4,0)
 ;;=4^M87.851
 ;;^UTILITY(U,$J,358.3,15143,2)
 ;;=^5014853
 ;;^UTILITY(U,$J,358.3,15144,0)
 ;;=M87.852^^61^746^75
 ;;^UTILITY(U,$J,358.3,15144,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15144,1,3,0)
 ;;=3^Osteonecrosis,Left Femur
 ;;^UTILITY(U,$J,358.3,15144,1,4,0)
 ;;=4^M87.852
 ;;^UTILITY(U,$J,358.3,15144,2)
 ;;=^5014854
 ;;^UTILITY(U,$J,358.3,15145,0)
 ;;=M88.0^^61^746^66
 ;;^UTILITY(U,$J,358.3,15145,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15145,1,3,0)
 ;;=3^Osteitis Deformans of Skull
 ;;^UTILITY(U,$J,358.3,15145,1,4,0)
 ;;=4^M88.0
 ;;^UTILITY(U,$J,358.3,15145,2)
 ;;=^5014874
 ;;^UTILITY(U,$J,358.3,15146,0)
 ;;=M88.1^^61^746^67
 ;;^UTILITY(U,$J,358.3,15146,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15146,1,3,0)
 ;;=3^Osteitis Deformans of Vertebrae
 ;;^UTILITY(U,$J,358.3,15146,1,4,0)
 ;;=4^M88.1
 ;;^UTILITY(U,$J,358.3,15146,2)
 ;;=^5014875
 ;;^UTILITY(U,$J,358.3,15147,0)
 ;;=M88.89^^61^746^65
 ;;^UTILITY(U,$J,358.3,15147,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15147,1,3,0)
 ;;=3^Osteitis Deformans of Mult Sites
 ;;^UTILITY(U,$J,358.3,15147,1,4,0)
 ;;=4^M88.89
 ;;^UTILITY(U,$J,358.3,15147,2)
 ;;=^5014898
 ;;^UTILITY(U,$J,358.3,15148,0)
 ;;=M94.0^^61^746^15
 ;;^UTILITY(U,$J,358.3,15148,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,15148,1,3,0)
 ;;=3^Chondrocostal Junction Syndrome/Costochondritis
 ;;^UTILITY(U,$J,358.3,15148,1,4,0)
 ;;=4^M94.0
 ;;^UTILITY(U,$J,358.3,15148,2)
 ;;=^5015327
 ;;^UTILITY(U,$J,358.3,15149,0)
 ;;=M94.20^^61^746^18
 ;;^UTILITY(U,$J,358.3,15149,1,0)
 ;;=^358.31IA^4^2
