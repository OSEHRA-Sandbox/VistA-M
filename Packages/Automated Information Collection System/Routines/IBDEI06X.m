IBDEI06X ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,9079,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9079,1,4,0)
 ;;=4^042.
 ;;^UTILITY(U,$J,358.3,9079,1,5,0)
 ;;=5^HIV Disease (symptomatic)
 ;;^UTILITY(U,$J,358.3,9079,2)
 ;;=HIV Disease (symptomatic)^266500
 ;;^UTILITY(U,$J,358.3,9080,0)
 ;;=464.00^^62^523^2
 ;;^UTILITY(U,$J,358.3,9080,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9080,1,4,0)
 ;;=4^464.00
 ;;^UTILITY(U,$J,358.3,9080,1,5,0)
 ;;=5^Acute Laryngitis
 ;;^UTILITY(U,$J,358.3,9080,2)
 ;;=Acute Laryngitis^323469
 ;;^UTILITY(U,$J,358.3,9081,0)
 ;;=790.6^^62^523^1
 ;;^UTILITY(U,$J,358.3,9081,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9081,1,4,0)
 ;;=4^790.6
 ;;^UTILITY(U,$J,358.3,9081,1,5,0)
 ;;=5^Abnormal LFT's
 ;;^UTILITY(U,$J,358.3,9081,2)
 ;;=^87228
 ;;^UTILITY(U,$J,358.3,9082,0)
 ;;=780.60^^62^523^20
 ;;^UTILITY(U,$J,358.3,9082,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9082,1,4,0)
 ;;=4^780.60
 ;;^UTILITY(U,$J,358.3,9082,1,5,0)
 ;;=5^Fever
 ;;^UTILITY(U,$J,358.3,9082,2)
 ;;=^336764
 ;;^UTILITY(U,$J,358.3,9083,0)
 ;;=795.51^^62^523^52
 ;;^UTILITY(U,$J,358.3,9083,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9083,1,4,0)
 ;;=4^795.51
 ;;^UTILITY(U,$J,358.3,9083,1,5,0)
 ;;=5^Pos PPD w/o Active TB
 ;;^UTILITY(U,$J,358.3,9083,2)
 ;;=^340572
 ;;^UTILITY(U,$J,358.3,9084,0)
 ;;=682.0^^62^524^6
 ;;^UTILITY(U,$J,358.3,9084,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9084,1,4,0)
 ;;=4^682.0
 ;;^UTILITY(U,$J,358.3,9084,1,5,0)
 ;;=5^Cellulitis Of Face
 ;;^UTILITY(U,$J,358.3,9084,2)
 ;;=^271888
 ;;^UTILITY(U,$J,358.3,9085,0)
 ;;=681.00^^62^524^7
 ;;^UTILITY(U,$J,358.3,9085,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9085,1,4,0)
 ;;=4^681.00
 ;;^UTILITY(U,$J,358.3,9085,1,5,0)
 ;;=5^Cellulitis Of Finger Nos
 ;;^UTILITY(U,$J,358.3,9085,2)
 ;;=^271883
 ;;^UTILITY(U,$J,358.3,9086,0)
 ;;=682.7^^62^524^8
 ;;^UTILITY(U,$J,358.3,9086,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9086,1,4,0)
 ;;=4^682.7
 ;;^UTILITY(U,$J,358.3,9086,1,5,0)
 ;;=5^Cellulitis Of Foot
 ;;^UTILITY(U,$J,358.3,9086,2)
 ;;=^271895
 ;;^UTILITY(U,$J,358.3,9087,0)
 ;;=682.4^^62^524^9
 ;;^UTILITY(U,$J,358.3,9087,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9087,1,4,0)
 ;;=4^682.4
 ;;^UTILITY(U,$J,358.3,9087,1,5,0)
 ;;=5^Cellulitis Of Hand
 ;;^UTILITY(U,$J,358.3,9087,2)
 ;;=^271892
 ;;^UTILITY(U,$J,358.3,9088,0)
 ;;=682.6^^62^524^10
 ;;^UTILITY(U,$J,358.3,9088,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9088,1,4,0)
 ;;=4^682.6
 ;;^UTILITY(U,$J,358.3,9088,1,5,0)
 ;;=5^Cellulitis Of Leg
 ;;^UTILITY(U,$J,358.3,9088,2)
 ;;=^271894
 ;;^UTILITY(U,$J,358.3,9089,0)
 ;;=682.1^^62^524^11
 ;;^UTILITY(U,$J,358.3,9089,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9089,1,4,0)
 ;;=4^682.1
 ;;^UTILITY(U,$J,358.3,9089,1,5,0)
 ;;=5^Cellulitis Of Neck
 ;;^UTILITY(U,$J,358.3,9089,2)
 ;;=^271889
 ;;^UTILITY(U,$J,358.3,9090,0)
 ;;=376.01^^62^524^12
 ;;^UTILITY(U,$J,358.3,9090,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9090,1,4,0)
 ;;=4^376.01
 ;;^UTILITY(U,$J,358.3,9090,1,5,0)
 ;;=5^Cellulitis Of Orbit
 ;;^UTILITY(U,$J,358.3,9090,2)
 ;;=^259068
 ;;^UTILITY(U,$J,358.3,9091,0)
 ;;=681.10^^62^524^13
 ;;^UTILITY(U,$J,358.3,9091,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9091,1,4,0)
 ;;=4^681.10
 ;;^UTILITY(U,$J,358.3,9091,1,5,0)
 ;;=5^Cellulitis Of Toe
 ;;^UTILITY(U,$J,358.3,9091,2)
 ;;=^271885
 ;;^UTILITY(U,$J,358.3,9092,0)
 ;;=682.2^^62^524^14
 ;;^UTILITY(U,$J,358.3,9092,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9092,1,4,0)
 ;;=4^682.2
 ;;^UTILITY(U,$J,358.3,9092,1,5,0)
 ;;=5^Cellulitis Of Trunk
 ;;^UTILITY(U,$J,358.3,9092,2)
 ;;=^271890
 ;;^UTILITY(U,$J,358.3,9093,0)
 ;;=691.8^^62^524^18
 ;;^UTILITY(U,$J,358.3,9093,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9093,1,4,0)
 ;;=4^691.8
 ;;^UTILITY(U,$J,358.3,9093,1,5,0)
 ;;=5^Dermatitis, Atopic
 ;;^UTILITY(U,$J,358.3,9093,2)
 ;;=^87342
 ;;^UTILITY(U,$J,358.3,9094,0)
 ;;=692.9^^62^524^28
 ;;^UTILITY(U,$J,358.3,9094,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9094,1,4,0)
 ;;=4^692.9
 ;;^UTILITY(U,$J,358.3,9094,1,5,0)
 ;;=5^Eczema, Allergic
 ;;^UTILITY(U,$J,358.3,9094,2)
 ;;=^27800
 ;;^UTILITY(U,$J,358.3,9095,0)
 ;;=693.0^^62^524^24
 ;;^UTILITY(U,$J,358.3,9095,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9095,1,4,0)
 ;;=4^693.0
 ;;^UTILITY(U,$J,358.3,9095,1,5,0)
 ;;=5^Drug Dermatitis Nos
 ;;^UTILITY(U,$J,358.3,9095,2)
 ;;=^33042
 ;;^UTILITY(U,$J,358.3,9096,0)
 ;;=459.81^^62^524^19
 ;;^UTILITY(U,$J,358.3,9096,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9096,1,4,0)
 ;;=4^459.81
 ;;^UTILITY(U,$J,358.3,9096,1,5,0)
 ;;=5^Dermatitis, Stasis
 ;;^UTILITY(U,$J,358.3,9096,2)
 ;;=^125826
 ;;^UTILITY(U,$J,358.3,9097,0)
 ;;=110.0^^62^524^61
 ;;^UTILITY(U,$J,358.3,9097,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9097,1,4,0)
 ;;=4^110.0
 ;;^UTILITY(U,$J,358.3,9097,1,5,0)
 ;;=5^Tinea Versicolor
 ;;^UTILITY(U,$J,358.3,9097,2)
 ;;=^33176
 ;;^UTILITY(U,$J,358.3,9098,0)
 ;;=110.5^^62^524^21
 ;;^UTILITY(U,$J,358.3,9098,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9098,1,4,0)
 ;;=4^110.5
 ;;^UTILITY(U,$J,358.3,9098,1,5,0)
 ;;=5^Dermatophytosis Of Body
 ;;^UTILITY(U,$J,358.3,9098,2)
 ;;=^33179
 ;;^UTILITY(U,$J,358.3,9099,0)
 ;;=110.6^^62^524^23
 ;;^UTILITY(U,$J,358.3,9099,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9099,1,4,0)
 ;;=4^110.6
 ;;^UTILITY(U,$J,358.3,9099,1,5,0)
 ;;=5^Dermatophytosis, Deep
 ;;^UTILITY(U,$J,358.3,9099,2)
 ;;=^266861
 ;;^UTILITY(U,$J,358.3,9100,0)
 ;;=110.4^^62^524^22
 ;;^UTILITY(U,$J,358.3,9100,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9100,1,4,0)
 ;;=4^110.4
 ;;^UTILITY(U,$J,358.3,9100,1,5,0)
 ;;=5^Dermatophytosis Of Foot
 ;;^UTILITY(U,$J,358.3,9100,2)
 ;;=^33168
 ;;^UTILITY(U,$J,358.3,9101,0)
 ;;=110.3^^62^524^60
 ;;^UTILITY(U,$J,358.3,9101,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9101,1,4,0)
 ;;=4^110.3
 ;;^UTILITY(U,$J,358.3,9101,1,5,0)
 ;;=5^Tinea Cruris
 ;;^UTILITY(U,$J,358.3,9101,2)
 ;;=^33171
 ;;^UTILITY(U,$J,358.3,9102,0)
 ;;=110.1^^62^524^37
 ;;^UTILITY(U,$J,358.3,9102,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9102,1,4,0)
 ;;=4^110.1
 ;;^UTILITY(U,$J,358.3,9102,1,5,0)
 ;;=5^Onychomycosis Finger Or Toe
 ;;^UTILITY(U,$J,358.3,9102,2)
 ;;=^33173
 ;;^UTILITY(U,$J,358.3,9103,0)
 ;;=707.13^^62^524^63
 ;;^UTILITY(U,$J,358.3,9103,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9103,1,4,0)
 ;;=4^707.13
 ;;^UTILITY(U,$J,358.3,9103,1,5,0)
 ;;=5^Ulcer of Ankle
 ;;^UTILITY(U,$J,358.3,9103,2)
 ;;=Ulcer of Ankle, non-diabetic^322145
 ;;^UTILITY(U,$J,358.3,9104,0)
 ;;=707.12^^62^524^64
 ;;^UTILITY(U,$J,358.3,9104,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9104,1,4,0)
 ;;=4^707.12
 ;;^UTILITY(U,$J,358.3,9104,1,5,0)
 ;;=5^Ulcer of Calf
 ;;^UTILITY(U,$J,358.3,9104,2)
 ;;=Ulcer of Calf, non-diabetic^322144
 ;;^UTILITY(U,$J,358.3,9105,0)
 ;;=707.15^^62^524^65
 ;;^UTILITY(U,$J,358.3,9105,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9105,1,4,0)
 ;;=4^707.15
 ;;^UTILITY(U,$J,358.3,9105,1,5,0)
 ;;=5^Ulcer of Foot
 ;;^UTILITY(U,$J,358.3,9105,2)
 ;;=Ulcer of Foot, non-diabetic^322148
 ;;^UTILITY(U,$J,358.3,9106,0)
 ;;=707.14^^62^524^66
 ;;^UTILITY(U,$J,358.3,9106,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9106,1,4,0)
 ;;=4^707.14
 ;;^UTILITY(U,$J,358.3,9106,1,5,0)
 ;;=5^Ulcer of Heel/Midfoot
 ;;^UTILITY(U,$J,358.3,9106,2)
 ;;=Ulcer of Heel/Midfoot, non-d-diabetic^322146
 ;;^UTILITY(U,$J,358.3,9107,0)
 ;;=707.10^^62^524^62
 ;;^UTILITY(U,$J,358.3,9107,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9107,1,4,0)
 ;;=4^707.10
 ;;^UTILITY(U,$J,358.3,9107,1,5,0)
 ;;=5^Ulcer Lower Extremity
 ;;^UTILITY(U,$J,358.3,9107,2)
 ;;=Ulcer, LE, non-diabetic^322142
 ;;^UTILITY(U,$J,358.3,9108,0)
 ;;=707.11^^62^524^67
 ;;^UTILITY(U,$J,358.3,9108,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9108,1,4,0)
 ;;=4^707.11
 ;;^UTILITY(U,$J,358.3,9108,1,5,0)
 ;;=5^Ulcer of Thigh
 ;;^UTILITY(U,$J,358.3,9108,2)
 ;;=Ulcer of Thigh, non-diabetic^322143
 ;;^UTILITY(U,$J,358.3,9109,0)
 ;;=695.3^^62^524^52
 ;;^UTILITY(U,$J,358.3,9109,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9109,1,4,0)
 ;;=4^695.3
 ;;^UTILITY(U,$J,358.3,9109,1,5,0)
 ;;=5^Rosacea
 ;;^UTILITY(U,$J,358.3,9109,2)
 ;;=^107114
 ;;^UTILITY(U,$J,358.3,9110,0)
 ;;=706.1^^62^524^1
 ;;^UTILITY(U,$J,358.3,9110,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9110,1,4,0)
 ;;=4^706.1
 ;;^UTILITY(U,$J,358.3,9110,1,5,0)
 ;;=5^Acne Vulgaris
 ;;^UTILITY(U,$J,358.3,9110,2)
 ;;=^87239
 ;;^UTILITY(U,$J,358.3,9111,0)
 ;;=702.0^^62^524^2
 ;;^UTILITY(U,$J,358.3,9111,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9111,1,4,0)
 ;;=4^702.0
 ;;^UTILITY(U,$J,358.3,9111,1,5,0)
 ;;=5^Actinic Keratosis
 ;;^UTILITY(U,$J,358.3,9111,2)
 ;;=^66900
 ;;^UTILITY(U,$J,358.3,9112,0)
 ;;=704.00^^62^524^3
 ;;^UTILITY(U,$J,358.3,9112,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9112,1,4,0)
 ;;=4^704.00
 ;;^UTILITY(U,$J,358.3,9112,1,5,0)
 ;;=5^Alopecia Nos
 ;;^UTILITY(U,$J,358.3,9112,2)
 ;;=^5078
 ;;^UTILITY(U,$J,358.3,9113,0)
 ;;=680.9^^62^524^5
 ;;^UTILITY(U,$J,358.3,9113,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9113,1,4,0)
 ;;=4^680.9
 ;;^UTILITY(U,$J,358.3,9113,1,5,0)
 ;;=5^Carbuncle/Furuncle
 ;;^UTILITY(U,$J,358.3,9113,2)
 ;;=^19191
 ;;^UTILITY(U,$J,358.3,9114,0)
 ;;=680.5^^62^524^4
 ;;^UTILITY(U,$J,358.3,9114,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9114,1,4,0)
 ;;=4^680.5
 ;;^UTILITY(U,$J,358.3,9114,1,5,0)
 ;;=5^Carbuncle Of Buttock
 ;;^UTILITY(U,$J,358.3,9114,2)
 ;;=^271878
 ;;^UTILITY(U,$J,358.3,9115,0)
 ;;=709.9^^62^524^59
 ;;^UTILITY(U,$J,358.3,9115,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9115,1,4,0)
 ;;=4^709.9
 ;;^UTILITY(U,$J,358.3,9115,1,5,0)
 ;;=5^Skin Lesion, Unsp
 ;;^UTILITY(U,$J,358.3,9115,2)
 ;;=^111083
 ;;^UTILITY(U,$J,358.3,9116,0)
 ;;=078.11^^62^524^15
 ;;^UTILITY(U,$J,358.3,9116,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,9116,1,4,0)
 ;;=4^078.11
 ;;^UTILITY(U,$J,358.3,9116,1,5,0)
 ;;=5^Condyloma Acuminatum
 ;;^UTILITY(U,$J,358.3,9116,2)
 ;;=^295788
 ;;^UTILITY(U,$J,358.3,9117,0)
 ;;=700.^^62^524^16
 ;;^UTILITY(U,$J,358.3,9117,1,0)
 ;;=^358.31IA^5^2
