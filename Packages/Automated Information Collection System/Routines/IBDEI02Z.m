IBDEI02Z ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,3560,0)
 ;;=162.4^^32^218^7
 ;;^UTILITY(U,$J,358.3,3560,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3560,1,4,0)
 ;;=4^162.4
 ;;^UTILITY(U,$J,358.3,3560,1,5,0)
 ;;=5^MAL NEO MIDDLE LOBE LUNG
 ;;^UTILITY(U,$J,358.3,3560,2)
 ;;=^267137
 ;;^UTILITY(U,$J,358.3,3561,0)
 ;;=162.5^^32^218^6
 ;;^UTILITY(U,$J,358.3,3561,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3561,1,4,0)
 ;;=4^162.5
 ;;^UTILITY(U,$J,358.3,3561,1,5,0)
 ;;=5^MAL NEO LOWER LOBE LUNG
 ;;^UTILITY(U,$J,358.3,3561,2)
 ;;=^267138
 ;;^UTILITY(U,$J,358.3,3562,0)
 ;;=162.8^^32^218^4
 ;;^UTILITY(U,$J,358.3,3562,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3562,1,4,0)
 ;;=4^162.8
 ;;^UTILITY(U,$J,358.3,3562,1,5,0)
 ;;=5^MAL NEO BRONCH/LUNG NEC
 ;;^UTILITY(U,$J,358.3,3562,2)
 ;;=^267139
 ;;^UTILITY(U,$J,358.3,3563,0)
 ;;=162.9^^32^218^5
 ;;^UTILITY(U,$J,358.3,3563,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3563,1,4,0)
 ;;=4^162.9
 ;;^UTILITY(U,$J,358.3,3563,1,5,0)
 ;;=5^MAL NEO BRONCH/LUNG NOS
 ;;^UTILITY(U,$J,358.3,3563,2)
 ;;=^73521
 ;;^UTILITY(U,$J,358.3,3564,0)
 ;;=793.11^^32^218^1
 ;;^UTILITY(U,$J,358.3,3564,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3564,1,4,0)
 ;;=4^793.11
 ;;^UTILITY(U,$J,358.3,3564,1,5,0)
 ;;=5^COIN LESION
 ;;^UTILITY(U,$J,358.3,3564,2)
 ;;=^340570
 ;;^UTILITY(U,$J,358.3,3565,0)
 ;;=512.81^^32^218^10
 ;;^UTILITY(U,$J,358.3,3565,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3565,1,4,0)
 ;;=4^512.81
 ;;^UTILITY(U,$J,358.3,3565,1,5,0)
 ;;=5^PRIM SPONT PNEUMOTHORAX
 ;;^UTILITY(U,$J,358.3,3565,2)
 ;;=^340529
 ;;^UTILITY(U,$J,358.3,3566,0)
 ;;=860.1^^32^218^14
 ;;^UTILITY(U,$J,358.3,3566,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3566,1,4,0)
 ;;=4^860.1
 ;;^UTILITY(U,$J,358.3,3566,1,5,0)
 ;;=5^TRAUM PNEUMOTHORAX-OPEN
 ;;^UTILITY(U,$J,358.3,3566,2)
 ;;=^274730
 ;;^UTILITY(U,$J,358.3,3567,0)
 ;;=512.0^^32^218^12
 ;;^UTILITY(U,$J,358.3,3567,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3567,1,4,0)
 ;;=4^512.0
 ;;^UTILITY(U,$J,358.3,3567,1,5,0)
 ;;=5^SPONT TENS PNEUMOTHORAX
 ;;^UTILITY(U,$J,358.3,3567,2)
 ;;=^269987
 ;;^UTILITY(U,$J,358.3,3568,0)
 ;;=860.0^^32^218^13
 ;;^UTILITY(U,$J,358.3,3568,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3568,1,4,0)
 ;;=4^860.0
 ;;^UTILITY(U,$J,358.3,3568,1,5,0)
 ;;=5^TRAUM PNEUMOTHORAX-CLOSE
 ;;^UTILITY(U,$J,358.3,3568,2)
 ;;=^274729
 ;;^UTILITY(U,$J,358.3,3569,0)
 ;;=512.1^^32^218^2
 ;;^UTILITY(U,$J,358.3,3569,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3569,1,4,0)
 ;;=4^512.1
 ;;^UTILITY(U,$J,358.3,3569,1,5,0)
 ;;=5^IATROGENIC PNEUMOTHORAX
 ;;^UTILITY(U,$J,358.3,3569,2)
 ;;=^303293
 ;;^UTILITY(U,$J,358.3,3570,0)
 ;;=327.26^^32^218^11
 ;;^UTILITY(U,$J,358.3,3570,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3570,1,4,0)
 ;;=4^327.26
 ;;^UTILITY(U,$J,358.3,3570,1,5,0)
 ;;=5^SLEEP HYPOVENT OTH DIS
 ;;^UTILITY(U,$J,358.3,3570,2)
 ;;=^332766
 ;;^UTILITY(U,$J,358.3,3571,0)
 ;;=491.22^^32^218^15
 ;;^UTILITY(U,$J,358.3,3571,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3571,1,4,0)
 ;;=4^491.22
 ;;^UTILITY(U,$J,358.3,3571,1,5,0)
 ;;=5^COPD W/ACUTE BRONCHITIS
 ;;^UTILITY(U,$J,358.3,3571,2)
 ;;=^331455
 ;;^UTILITY(U,$J,358.3,3572,0)
 ;;=491.21^^32^218^16
 ;;^UTILITY(U,$J,358.3,3572,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3572,1,4,0)
 ;;=4^491.21
 ;;^UTILITY(U,$J,358.3,3572,1,5,0)
 ;;=5^COPD W/CHR BRONCHITIS,AC EXAC
 ;;^UTILITY(U,$J,358.3,3572,2)
 ;;=^330084
 ;;^UTILITY(U,$J,358.3,3573,0)
 ;;=491.20^^32^218^17
 ;;^UTILITY(U,$J,358.3,3573,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3573,1,4,0)
 ;;=4^491.20
 ;;^UTILITY(U,$J,358.3,3573,1,5,0)
 ;;=5^COPD W/CHR BRONCHITIS,STABLE
 ;;^UTILITY(U,$J,358.3,3573,2)
 ;;=^330083
 ;;^UTILITY(U,$J,358.3,3574,0)
 ;;=359.9^^32^219^2
 ;;^UTILITY(U,$J,358.3,3574,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3574,1,4,0)
 ;;=4^359.9
 ;;^UTILITY(U,$J,358.3,3574,1,5,0)
 ;;=5^MYOPATHY NOS
 ;;^UTILITY(U,$J,358.3,3574,2)
 ;;=^79927
 ;;^UTILITY(U,$J,358.3,3575,0)
 ;;=358.00^^32^219^1
 ;;^UTILITY(U,$J,358.3,3575,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3575,1,4,0)
 ;;=4^358.00
 ;;^UTILITY(U,$J,358.3,3575,1,5,0)
 ;;=5^MYASTHENIA GRAVIS
 ;;^UTILITY(U,$J,358.3,3575,2)
 ;;=^329920
 ;;^UTILITY(U,$J,358.3,3576,0)
 ;;=519.4^^32^219^3
 ;;^UTILITY(U,$J,358.3,3576,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3576,1,4,0)
 ;;=4^519.4
 ;;^UTILITY(U,$J,358.3,3576,1,5,0)
 ;;=5^PARALYSIS,DIAPHRAGM
 ;;^UTILITY(U,$J,358.3,3576,2)
 ;;=^265105
 ;;^UTILITY(U,$J,358.3,3577,0)
 ;;=344.00^^32^219^5
 ;;^UTILITY(U,$J,358.3,3577,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3577,1,4,0)
 ;;=4^344.00
 ;;^UTILITY(U,$J,358.3,3577,1,5,0)
 ;;=5^QUADRIPLEGIA, UNSP
 ;;^UTILITY(U,$J,358.3,3577,2)
 ;;=^101908
 ;;^UTILITY(U,$J,358.3,3578,0)
 ;;=344.1^^32^219^4
 ;;^UTILITY(U,$J,358.3,3578,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3578,1,4,0)
 ;;=4^344.1
 ;;^UTILITY(U,$J,358.3,3578,1,5,0)
 ;;=5^PARAPLEGIA NOS
 ;;^UTILITY(U,$J,358.3,3578,2)
 ;;=^90028
 ;;^UTILITY(U,$J,358.3,3579,0)
 ;;=501.^^32^220^1
 ;;^UTILITY(U,$J,358.3,3579,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3579,1,4,0)
 ;;=4^501.
 ;;^UTILITY(U,$J,358.3,3579,1,5,0)
 ;;=5^ASBESTOSIS
 ;;^UTILITY(U,$J,358.3,3579,2)
 ;;=^10704
 ;;^UTILITY(U,$J,358.3,3580,0)
 ;;=500.^^32^220^2
 ;;^UTILITY(U,$J,358.3,3580,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3580,1,4,0)
 ;;=4^500.
 ;;^UTILITY(U,$J,358.3,3580,1,5,0)
 ;;=5^COAL WORKERS' PNEUMOCON
 ;;^UTILITY(U,$J,358.3,3580,2)
 ;;=^8060
 ;;^UTILITY(U,$J,358.3,3581,0)
 ;;=505.^^32^220^3
 ;;^UTILITY(U,$J,358.3,3581,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3581,1,4,0)
 ;;=4^505.
 ;;^UTILITY(U,$J,358.3,3581,1,5,0)
 ;;=5^PNEUMOCONIOSIS NOS
 ;;^UTILITY(U,$J,358.3,3581,2)
 ;;=^95539
 ;;^UTILITY(U,$J,358.3,3582,0)
 ;;=502.^^32^220^4
 ;;^UTILITY(U,$J,358.3,3582,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3582,1,4,0)
 ;;=4^502.
 ;;^UTILITY(U,$J,358.3,3582,1,5,0)
 ;;=5^SILICOSIS
 ;;^UTILITY(U,$J,358.3,3582,2)
 ;;=^110600
 ;;^UTILITY(U,$J,358.3,3583,0)
 ;;=501.^^32^221^1
 ;;^UTILITY(U,$J,358.3,3583,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3583,1,4,0)
 ;;=4^501.
 ;;^UTILITY(U,$J,358.3,3583,1,5,0)
 ;;=5^ASBESTOS PLAQUES
 ;;^UTILITY(U,$J,358.3,3583,2)
 ;;=^10704
 ;;^UTILITY(U,$J,358.3,3584,0)
 ;;=510.9^^32^221^2
 ;;^UTILITY(U,$J,358.3,3584,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3584,1,4,0)
 ;;=4^510.9
 ;;^UTILITY(U,$J,358.3,3584,1,5,0)
 ;;=5^EMPYEMA W/O FISTULA
 ;;^UTILITY(U,$J,358.3,3584,2)
 ;;=^39810
 ;;^UTILITY(U,$J,358.3,3585,0)
 ;;=511.9^^32^221^3
 ;;^UTILITY(U,$J,358.3,3585,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3585,1,4,0)
 ;;=4^511.9
 ;;^UTILITY(U,$J,358.3,3585,1,5,0)
 ;;=5^PLEURAL EFFUSION NOS
 ;;^UTILITY(U,$J,358.3,3585,2)
 ;;=^123973
 ;;^UTILITY(U,$J,358.3,3586,0)
 ;;=163.9^^32^221^4
 ;;^UTILITY(U,$J,358.3,3586,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3586,1,4,0)
 ;;=4^163.9
 ;;^UTILITY(U,$J,358.3,3586,1,5,0)
 ;;=5^PLEURAL MALIGNANCY
 ;;^UTILITY(U,$J,358.3,3586,2)
 ;;=^267140
 ;;^UTILITY(U,$J,358.3,3587,0)
 ;;=511.0^^32^221^5
 ;;^UTILITY(U,$J,358.3,3587,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3587,1,4,0)
 ;;=4^511.0
 ;;^UTILITY(U,$J,358.3,3587,1,5,0)
 ;;=5^PLEURISY
 ;;^UTILITY(U,$J,358.3,3587,2)
 ;;=^95432
 ;;^UTILITY(U,$J,358.3,3588,0)
 ;;=508.1^^32^222^1
 ;;^UTILITY(U,$J,358.3,3588,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3588,1,4,0)
 ;;=4^508.1
 ;;^UTILITY(U,$J,358.3,3588,1,5,0)
 ;;=5^RADIATION FIBROSIS
 ;;^UTILITY(U,$J,358.3,3588,2)
 ;;=^269983
 ;;^UTILITY(U,$J,358.3,3589,0)
 ;;=508.0^^32^222^2
 ;;^UTILITY(U,$J,358.3,3589,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3589,1,4,0)
 ;;=4^508.0
 ;;^UTILITY(U,$J,358.3,3589,1,5,0)
 ;;=5^RADIATION PNEUMONITIS
 ;;^UTILITY(U,$J,358.3,3589,2)
 ;;=^269982
 ;;^UTILITY(U,$J,358.3,3590,0)
 ;;=733.40^^32^223^1
 ;;^UTILITY(U,$J,358.3,3590,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3590,1,4,0)
 ;;=4^733.40
 ;;^UTILITY(U,$J,358.3,3590,1,5,0)
 ;;=5^ASEPT NECROSIS BONE NOS
 ;;^UTILITY(U,$J,358.3,3590,2)
 ;;=^276833
 ;;^UTILITY(U,$J,358.3,3591,0)
 ;;=737.34^^32^223^2
 ;;^UTILITY(U,$J,358.3,3591,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3591,1,4,0)
 ;;=4^737.34
 ;;^UTILITY(U,$J,358.3,3591,1,5,0)
 ;;=5^KYPHOSCOLIOSIS,THORAX
 ;;^UTILITY(U,$J,358.3,3591,2)
 ;;=^272762
 ;;^UTILITY(U,$J,358.3,3592,0)
 ;;=733.90^^32^223^3
 ;;^UTILITY(U,$J,358.3,3592,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3592,1,4,0)
 ;;=4^733.90
 ;;^UTILITY(U,$J,358.3,3592,1,5,0)
 ;;=5^OSTEOPENIA
 ;;^UTILITY(U,$J,358.3,3592,2)
 ;;=^35593
 ;;^UTILITY(U,$J,358.3,3593,0)
 ;;=733.00^^32^223^4
 ;;^UTILITY(U,$J,358.3,3593,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3593,1,4,0)
 ;;=4^733.00
 ;;^UTILITY(U,$J,358.3,3593,1,5,0)
 ;;=5^OSTEOPOROSIS NOS
 ;;^UTILITY(U,$J,358.3,3593,2)
 ;;=^87159
 ;;^UTILITY(U,$J,358.3,3594,0)
 ;;=807.00^^32^223^5
 ;;^UTILITY(U,$J,358.3,3594,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3594,1,4,0)
 ;;=4^807.00
 ;;^UTILITY(U,$J,358.3,3594,1,5,0)
 ;;=5^RIB FRACTURE,CLOSED
 ;;^UTILITY(U,$J,358.3,3594,2)
 ;;=^25317
 ;;^UTILITY(U,$J,358.3,3595,0)
 ;;=733.13^^32^223^6
 ;;^UTILITY(U,$J,358.3,3595,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3595,1,4,0)
 ;;=4^733.13
 ;;^UTILITY(U,$J,358.3,3595,1,5,0)
 ;;=5^VERTEBRAL COMPRESSION FX
 ;;^UTILITY(U,$J,358.3,3595,2)
 ;;=^295754
 ;;^UTILITY(U,$J,358.3,3596,0)
 ;;=790.6^^32^224^1
 ;;^UTILITY(U,$J,358.3,3596,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3596,1,4,0)
 ;;=4^790.6
 ;;^UTILITY(U,$J,358.3,3596,1,5,0)
 ;;=5^ABNORMAL LFTs
 ;;^UTILITY(U,$J,358.3,3596,2)
 ;;=^87228
 ;;^UTILITY(U,$J,358.3,3597,0)
 ;;=571.5^^32^224^2
 ;;^UTILITY(U,$J,358.3,3597,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3597,1,4,0)
 ;;=4^571.5
 ;;^UTILITY(U,$J,358.3,3597,1,5,0)
 ;;=5^CIRRHOSIS OF LIVER NOS
 ;;^UTILITY(U,$J,358.3,3597,2)
 ;;=^24731
 ;;^UTILITY(U,$J,358.3,3598,0)
 ;;=V58.32^^32^225^1
 ;;^UTILITY(U,$J,358.3,3598,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,3598,1,4,0)
 ;;=4^V58.32
 ;;^UTILITY(U,$J,358.3,3598,1,5,0)
 ;;=5^REMOVAL OF SUTURES
