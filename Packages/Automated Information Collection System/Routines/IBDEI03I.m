IBDEI03I ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,1286,0)
 ;;=33217^^14^129^21^^^^1
 ;;^UTILITY(U,$J,358.3,1286,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1286,1,2,0)
 ;;=2^33217
 ;;^UTILITY(U,$J,358.3,1286,1,3,0)
 ;;=3^Insert/Reposit Transv Elec(Dual)
 ;;^UTILITY(U,$J,358.3,1287,0)
 ;;=33218^^14^129^48^^^^1
 ;;^UTILITY(U,$J,358.3,1287,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1287,1,2,0)
 ;;=2^33218
 ;;^UTILITY(U,$J,358.3,1287,1,3,0)
 ;;=3^Repair Transv Elec (Single)
 ;;^UTILITY(U,$J,358.3,1288,0)
 ;;=33220^^14^129^47^^^^1
 ;;^UTILITY(U,$J,358.3,1288,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1288,1,2,0)
 ;;=2^33220
 ;;^UTILITY(U,$J,358.3,1288,1,3,0)
 ;;=3^Repair Transv Elec (Dual)
 ;;^UTILITY(U,$J,358.3,1289,0)
 ;;=33222^^14^129^50^^^^1
 ;;^UTILITY(U,$J,358.3,1289,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1289,1,2,0)
 ;;=2^33222
 ;;^UTILITY(U,$J,358.3,1289,1,3,0)
 ;;=3^Revis Or Reloc Skin Pckt
 ;;^UTILITY(U,$J,358.3,1290,0)
 ;;=33233^^14^129^43^^^^1
 ;;^UTILITY(U,$J,358.3,1290,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1290,1,2,0)
 ;;=2^33233
 ;;^UTILITY(U,$J,358.3,1290,1,3,0)
 ;;=3^Remove Pace Pulse Gen
 ;;^UTILITY(U,$J,358.3,1291,0)
 ;;=92960^^14^129^4^^^^1
 ;;^UTILITY(U,$J,358.3,1291,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1291,1,2,0)
 ;;=2^92960
 ;;^UTILITY(U,$J,358.3,1291,1,3,0)
 ;;=3^Cardioversion
 ;;^UTILITY(U,$J,358.3,1292,0)
 ;;=93650^^14^129^1^^^^1
 ;;^UTILITY(U,$J,358.3,1292,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1292,1,2,0)
 ;;=2^93650
 ;;^UTILITY(U,$J,358.3,1292,1,3,0)
 ;;=3^Abalation, Av Node
 ;;^UTILITY(U,$J,358.3,1293,0)
 ;;=93740^^14^129^53^^^^1
 ;;^UTILITY(U,$J,358.3,1293,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1293,1,2,0)
 ;;=2^93740
 ;;^UTILITY(U,$J,358.3,1293,1,3,0)
 ;;=3^Temp Gradient Studies
 ;;^UTILITY(U,$J,358.3,1294,0)
 ;;=33234^^14^129^38^^^^1
 ;;^UTILITY(U,$J,358.3,1294,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1294,1,2,0)
 ;;=2^33234
 ;;^UTILITY(U,$J,358.3,1294,1,3,0)
 ;;=3^Rem Transv Elec Atria/Vent(Sgl)
 ;;^UTILITY(U,$J,358.3,1295,0)
 ;;=33235^^14^129^37^^^^1
 ;;^UTILITY(U,$J,358.3,1295,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1295,1,2,0)
 ;;=2^33235
 ;;^UTILITY(U,$J,358.3,1295,1,3,0)
 ;;=3^Rem Transv Elec Atria/Vent(Dual)
 ;;^UTILITY(U,$J,358.3,1296,0)
 ;;=33240^^14^129^19^^^^1
 ;;^UTILITY(U,$J,358.3,1296,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1296,1,2,0)
 ;;=2^33240
 ;;^UTILITY(U,$J,358.3,1296,1,3,0)
 ;;=3^Insert Single/Dual Pulse Gen
 ;;^UTILITY(U,$J,358.3,1297,0)
 ;;=33241^^14^129^52^^^^1
 ;;^UTILITY(U,$J,358.3,1297,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1297,1,2,0)
 ;;=2^33241
 ;;^UTILITY(U,$J,358.3,1297,1,3,0)
 ;;=3^Subq Remove Sgl/Dual Pulse Gen
 ;;^UTILITY(U,$J,358.3,1298,0)
 ;;=33244^^14^129^56^^^^1
 ;;^UTILITY(U,$J,358.3,1298,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1298,1,2,0)
 ;;=2^33244
 ;;^UTILITY(U,$J,358.3,1298,1,3,0)
 ;;=3^Transv Remove Sgl/Dual Elec
 ;;^UTILITY(U,$J,358.3,1299,0)
 ;;=33249^^14^129^14^^^^1
 ;;^UTILITY(U,$J,358.3,1299,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1299,1,2,0)
 ;;=2^33249
 ;;^UTILITY(U,$J,358.3,1299,1,3,0)
 ;;=3^Ins/Reposit Lead, Insert Pulse Gen
 ;;^UTILITY(U,$J,358.3,1300,0)
 ;;=93285^^14^129^12^^^^1
 ;;^UTILITY(U,$J,358.3,1300,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1300,1,2,0)
 ;;=2^93285
 ;;^UTILITY(U,$J,358.3,1300,1,3,0)
 ;;=3^ILR Device Eval Progr
 ;;^UTILITY(U,$J,358.3,1301,0)
 ;;=93291^^14^129^13^^^^1
 ;;^UTILITY(U,$J,358.3,1301,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1301,1,2,0)
 ;;=2^93291
 ;;^UTILITY(U,$J,358.3,1301,1,3,0)
 ;;=3^ILR Device Interrogate
 ;;^UTILITY(U,$J,358.3,1302,0)
 ;;=93294^^14^129^27^^^^1
