IBDEI0A2 ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,10066,1,4,0)
 ;;=4^H01.004
 ;;^UTILITY(U,$J,358.3,10066,2)
 ;;=^5004241
 ;;^UTILITY(U,$J,358.3,10067,0)
 ;;=H01.005^^51^588^5
 ;;^UTILITY(U,$J,358.3,10067,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10067,1,3,0)
 ;;=3^Blepharitis,Left Lower Eyelid,Unspec
 ;;^UTILITY(U,$J,358.3,10067,1,4,0)
 ;;=4^H01.005
 ;;^UTILITY(U,$J,358.3,10067,2)
 ;;=^5133380
 ;;^UTILITY(U,$J,358.3,10068,0)
 ;;=H02.105^^51^588^24
 ;;^UTILITY(U,$J,358.3,10068,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10068,1,3,0)
 ;;=3^Ectropion,Left Lower Eyelid,Unspec
 ;;^UTILITY(U,$J,358.3,10068,1,4,0)
 ;;=4^H02.105
 ;;^UTILITY(U,$J,358.3,10068,2)
 ;;=^5133409
 ;;^UTILITY(U,$J,358.3,10069,0)
 ;;=H02.104^^51^588^25
 ;;^UTILITY(U,$J,358.3,10069,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10069,1,3,0)
 ;;=3^Ectropion,Left Upper Eyelid,Unspec
 ;;^UTILITY(U,$J,358.3,10069,1,4,0)
 ;;=4^H02.104
 ;;^UTILITY(U,$J,358.3,10069,2)
 ;;=^5004305
 ;;^UTILITY(U,$J,358.3,10070,0)
 ;;=H02.101^^51^588^27
 ;;^UTILITY(U,$J,358.3,10070,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10070,1,3,0)
 ;;=3^Ectropion,Right Upper Eyelid,Unspec
 ;;^UTILITY(U,$J,358.3,10070,1,4,0)
 ;;=4^H02.101
 ;;^UTILITY(U,$J,358.3,10070,2)
 ;;=^5004303
 ;;^UTILITY(U,$J,358.3,10071,0)
 ;;=H02.102^^51^588^26
 ;;^UTILITY(U,$J,358.3,10071,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10071,1,3,0)
 ;;=3^Ectropion,Right Lower Eyelid,Unspec
 ;;^UTILITY(U,$J,358.3,10071,1,4,0)
 ;;=4^H02.102
 ;;^UTILITY(U,$J,358.3,10071,2)
 ;;=^5133407
 ;;^UTILITY(U,$J,358.3,10072,0)
 ;;=H04.121^^51^588^23
 ;;^UTILITY(U,$J,358.3,10072,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10072,1,3,0)
 ;;=3^Dry Eye Syndrome,Right Lacrimal Gland
 ;;^UTILITY(U,$J,358.3,10072,1,4,0)
 ;;=4^H04.121
 ;;^UTILITY(U,$J,358.3,10072,2)
 ;;=^5004463
 ;;^UTILITY(U,$J,358.3,10073,0)
 ;;=H04.122^^51^588^22
 ;;^UTILITY(U,$J,358.3,10073,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10073,1,3,0)
 ;;=3^Dry Eye Syndrome,Left Lacrimal Gland
 ;;^UTILITY(U,$J,358.3,10073,1,4,0)
 ;;=4^H04.122
 ;;^UTILITY(U,$J,358.3,10073,2)
 ;;=^5004464
 ;;^UTILITY(U,$J,358.3,10074,0)
 ;;=H43.811^^51^588^80
 ;;^UTILITY(U,$J,358.3,10074,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10074,1,3,0)
 ;;=3^Vitreous Degeneration,Right Eye
 ;;^UTILITY(U,$J,358.3,10074,1,4,0)
 ;;=4^H43.811
 ;;^UTILITY(U,$J,358.3,10074,2)
 ;;=^5005953
 ;;^UTILITY(U,$J,358.3,10075,0)
 ;;=H43.812^^51^588^79
 ;;^UTILITY(U,$J,358.3,10075,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10075,1,3,0)
 ;;=3^Vitreous Degeneration,Left Eye
 ;;^UTILITY(U,$J,358.3,10075,1,4,0)
 ;;=4^H43.812
 ;;^UTILITY(U,$J,358.3,10075,2)
 ;;=^5005954
 ;;^UTILITY(U,$J,358.3,10076,0)
 ;;=H25.89^^51^588^9
 ;;^UTILITY(U,$J,358.3,10076,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10076,1,3,0)
 ;;=3^Cataract,Age-Related NEC
 ;;^UTILITY(U,$J,358.3,10076,1,4,0)
 ;;=4^H25.89
 ;;^UTILITY(U,$J,358.3,10076,2)
 ;;=^5005295
 ;;^UTILITY(U,$J,358.3,10077,0)
 ;;=H53.40^^51^588^78
 ;;^UTILITY(U,$J,358.3,10077,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10077,1,3,0)
 ;;=3^Visual Field Defects,Unspec
 ;;^UTILITY(U,$J,358.3,10077,1,4,0)
 ;;=4^H53.40
 ;;^UTILITY(U,$J,358.3,10077,2)
 ;;=^5006324
 ;;^UTILITY(U,$J,358.3,10078,0)
 ;;=H01.001^^51^588^8
 ;;^UTILITY(U,$J,358.3,10078,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10078,1,3,0)
 ;;=3^Blepharitis,Right Upper Eyelid,Unspec
 ;;^UTILITY(U,$J,358.3,10078,1,4,0)
 ;;=4^H01.001
 ;;^UTILITY(U,$J,358.3,10078,2)
 ;;=^5004238
 ;;^UTILITY(U,$J,358.3,10079,0)
 ;;=H01.002^^51^588^7
 ;;^UTILITY(U,$J,358.3,10079,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10079,1,3,0)
 ;;=3^Blepharitis,Right Lower Eyelid,Unspec
 ;;^UTILITY(U,$J,358.3,10079,1,4,0)
 ;;=4^H01.002
 ;;^UTILITY(U,$J,358.3,10079,2)
 ;;=^5004239
 ;;^UTILITY(U,$J,358.3,10080,0)
 ;;=H25.13^^51^588^10
 ;;^UTILITY(U,$J,358.3,10080,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10080,1,3,0)
 ;;=3^Cataract,Age-Related Nuclear,Bilateral
 ;;^UTILITY(U,$J,358.3,10080,1,4,0)
 ;;=4^H25.13
 ;;^UTILITY(U,$J,358.3,10080,2)
 ;;=^5005286
 ;;^UTILITY(U,$J,358.3,10081,0)
 ;;=H25.12^^51^588^11
 ;;^UTILITY(U,$J,358.3,10081,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10081,1,3,0)
 ;;=3^Cataract,Age-Related Nuclear,Left Eye
 ;;^UTILITY(U,$J,358.3,10081,1,4,0)
 ;;=4^H25.12
 ;;^UTILITY(U,$J,358.3,10081,2)
 ;;=^5005285
 ;;^UTILITY(U,$J,358.3,10082,0)
 ;;=H25.11^^51^588^12
 ;;^UTILITY(U,$J,358.3,10082,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10082,1,3,0)
 ;;=3^Cataract,Age-Related Nuclear,Right Eye
 ;;^UTILITY(U,$J,358.3,10082,1,4,0)
 ;;=4^H25.11
 ;;^UTILITY(U,$J,358.3,10082,2)
 ;;=^5005284
 ;;^UTILITY(U,$J,358.3,10083,0)
 ;;=G45.3^^51^588^1
 ;;^UTILITY(U,$J,358.3,10083,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10083,1,3,0)
 ;;=3^Amaurosis Fugax
 ;;^UTILITY(U,$J,358.3,10083,1,4,0)
 ;;=4^G45.3
 ;;^UTILITY(U,$J,358.3,10083,2)
 ;;=^304129
 ;;^UTILITY(U,$J,358.3,10084,0)
 ;;=D31.32^^51^588^16
 ;;^UTILITY(U,$J,358.3,10084,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10084,1,3,0)
 ;;=3^Choroidal Nevus,Left Eye
 ;;^UTILITY(U,$J,358.3,10084,1,4,0)
 ;;=4^D31.32
 ;;^UTILITY(U,$J,358.3,10084,2)
 ;;=^5002122
 ;;^UTILITY(U,$J,358.3,10085,0)
 ;;=D31.31^^51^588^17
 ;;^UTILITY(U,$J,358.3,10085,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10085,1,3,0)
 ;;=3^Choroidal Nevus,Right Eye
 ;;^UTILITY(U,$J,358.3,10085,1,4,0)
 ;;=4^D31.31
 ;;^UTILITY(U,$J,358.3,10085,2)
 ;;=^5002121
 ;;^UTILITY(U,$J,358.3,10086,0)
 ;;=H34.813^^51^588^13
 ;;^UTILITY(U,$J,358.3,10086,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10086,1,3,0)
 ;;=3^Central Retinal Vein Occlusion,Bilateral
 ;;^UTILITY(U,$J,358.3,10086,1,4,0)
 ;;=4^H34.813
 ;;^UTILITY(U,$J,358.3,10086,2)
 ;;=^5005570
 ;;^UTILITY(U,$J,358.3,10087,0)
 ;;=H34.812^^51^588^14
 ;;^UTILITY(U,$J,358.3,10087,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10087,1,3,0)
 ;;=3^Central Retinal Vein Occlusion,Left Eye
 ;;^UTILITY(U,$J,358.3,10087,1,4,0)
 ;;=4^H34.812
 ;;^UTILITY(U,$J,358.3,10087,2)
 ;;=^5005569
 ;;^UTILITY(U,$J,358.3,10088,0)
 ;;=H34.811^^51^588^15
 ;;^UTILITY(U,$J,358.3,10088,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10088,1,3,0)
 ;;=3^Central Retinal Vein Occlusion,Right Eye
 ;;^UTILITY(U,$J,358.3,10088,1,4,0)
 ;;=4^H34.811
 ;;^UTILITY(U,$J,358.3,10088,2)
 ;;=^5005568
 ;;^UTILITY(U,$J,358.3,10089,0)
 ;;=E11.321^^51^588^66
 ;;^UTILITY(U,$J,358.3,10089,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10089,1,3,0)
 ;;=3^Type 2 DM w/ Mild Nonprolif DM Retinopathy w/ Macular Edema
 ;;^UTILITY(U,$J,358.3,10089,1,4,0)
 ;;=4^E11.321
 ;;^UTILITY(U,$J,358.3,10089,2)
 ;;=^5002634
 ;;^UTILITY(U,$J,358.3,10090,0)
 ;;=E11.329^^51^588^67
 ;;^UTILITY(U,$J,358.3,10090,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10090,1,3,0)
 ;;=3^Type 2 DM w/ Mild Nonprolif DM Retinopathy w/o Macular Edema
 ;;^UTILITY(U,$J,358.3,10090,1,4,0)
 ;;=4^E11.329
 ;;^UTILITY(U,$J,358.3,10090,2)
 ;;=^5002635
 ;;^UTILITY(U,$J,358.3,10091,0)
 ;;=E11.331^^51^588^68
 ;;^UTILITY(U,$J,358.3,10091,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10091,1,3,0)
 ;;=3^Type 2 DM w/ Mod Nonprolif DM Retinopathy w/ Macular Edema
 ;;^UTILITY(U,$J,358.3,10091,1,4,0)
 ;;=4^E11.331
 ;;^UTILITY(U,$J,358.3,10091,2)
 ;;=^5002636
 ;;^UTILITY(U,$J,358.3,10092,0)
 ;;=E11.339^^51^588^69
 ;;^UTILITY(U,$J,358.3,10092,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10092,1,3,0)
 ;;=3^Type 2 DM w/ Mod Nonprolif DM Retinopathy w/o Macular Edema
 ;;^UTILITY(U,$J,358.3,10092,1,4,0)
 ;;=4^E11.339
 ;;^UTILITY(U,$J,358.3,10092,2)
 ;;=^5002637
 ;;^UTILITY(U,$J,358.3,10093,0)
 ;;=E11.351^^51^588^70
 ;;^UTILITY(U,$J,358.3,10093,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10093,1,3,0)
 ;;=3^Type 2 DM w/ Prolif DM Retinopathy w/ Macular Edema
 ;;^UTILITY(U,$J,358.3,10093,1,4,0)
 ;;=4^E11.351
 ;;^UTILITY(U,$J,358.3,10093,2)
 ;;=^5002640
 ;;^UTILITY(U,$J,358.3,10094,0)
 ;;=E11.359^^51^588^71
