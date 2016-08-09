ORKCHK6 ; SLC/CLA - Support routine called by ORKCHK to do SESSION mode order checks ;08/18/14  08:37
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**6,32,74,87,94,123,162,190,249,280,272,346,345,269**;Dec 17, 1997;Build 85
 Q
 ;
EN(ORKS,ORKDFN,ORKA,ORENT,ORKTMODE) ;perform order checking for entire ordering session
 Q:$$GET^XPAR("DIV^SYS^PKG","ORK SYSTEM ENABLE/DISABLE",1,"I")="D"
 ;
 N OI,ORKDG,HL7,ODT,ORNUM,HL7NPTR,HL7NTXT,HL7NCOD,HL7LPTR,HL7LTXT,HL7LCOD
 N ORKMSG,ORKDGI,ORKTXT,ORKPDATA,ORIFN
 ;
 S OI=$P(ORKA,"|"),ORKDG=$P(ORKA,"|",2),HL7=$P(ORKA,"|",3)
 S ODT=$P(ORKA,"|",4),ORNUM=$P(ORKA,"|",5),ORKPDATA=$P(ORKA,"|",6)
 S HL7NPTR=$P(HL7,U),HL7NTXT=$P(HL7,U,2),HL7NCOD=$P(HL7,U,3)
 S HL7LPTR=$P(HL7,U,4),HL7LTXT=$P(HL7,U,5),HL7LCOD=$P(HL7,U,6)
 S ORIFN=ORNUM
 ;
 S:ORKDG="PSJ" ORKDG="PSI"
 I $E(ORKDG,1,2)="PS" D PHARM
 I $E(ORKDG,1,2)'="PS" D MLM^ORKCHK2(.ORKS,ORKDFN,ORKA,ORENT,"SESSION")
 Q
 ;
PHARM ;process pharmacy order checks:
 N ORPSPKG,ORPSA,ORRXDONE
 N ORCRITN,ORCRITF,ORCRITD,ORSIGN,ORSIGF,ORSIGD,ORDUPN,ORDUPF,ORDUPD,ORDUPC,ORDUPCF,ORDUPCD,ORALLRN,ORALLRF,ORALLRD,ORDUPCN
 ;
 D PARAMS("ALLERGY-DRUG INTERACTION",.ORALLRN,.ORALLRF,.ORALLRD)
 D PARAMS("CRITICAL DRUG INTERACTION",.ORCRITN,.ORCRITF,.ORCRITD)
 D PARAMS("SIGNIFICANT DRUG INTERACTION",.ORSIGN,.ORSIGF,.ORSIGD)
 D PARAMS("DUPLICATE DRUG ORDER",.ORDUPN,.ORDUPF,.ORDUPD)
 D PARAMS("DUPLICATE DRUG THERAPY",.ORDUPCN,.ORDUPCF,.ORDUPCD)
 ;
 S ORRXDONE=0 ;flag to set if RXOCS gets called
 ;dispense drug selected:
 I $L($G(HL7LPTR)),($G(HL7LCOD)="99PSD") D
 .D RXOCS
 .S:$P(ORKA,"|",7)'="" $P(ORKA,"|",7)=""
 .S ORRXDONE=1
 .D MLM^ORKCHK2(.ORKS,ORKDFN,ORKA,ORENT,"SESSION")
 ;
 ;dispense drug NOT selected, split OI into dispense drugs:
 I '$L($G(HL7LPTR)) D
 .S ORPSPKG=$E(ORKDG,3)
 .I "IOH"[ORPSPKG D
 ..S ORPSA=$$OI2DD^ORKPS(OI,ORPSPKG,1)
 ..I +ORPSA D
 ...S HL7LTXT=$P($G(^ORD(101.43,OI,0)),U)
 ...S HL7NPTR=$P(ORPSA,";",2)
 ...S HL7LPTR=+ORPSA
 ...S HL7LCOD="99PSD",HL7NCOD="99NDF"
 ...S $P(HL7,U)=HL7NPTR,$P(HL7,U,3)=HL7NCOD
 ...S $P(HL7,U,4)=HL7LPTR,$P(HL7,U,5)=HL7LTXT,$P(HL7,U,6)=HL7LCOD
 ...S $P(ORKA,"|",3)=HL7  ;set these for MLM OCX call
 ..D RXOCS
 ..S:$P(ORKA,"|",7)'="" $P(ORKA,"|",7)=""
 ..S ORRXDONE=1
 ..Q:HL7LTXT=""
 ..D MLM^ORKCHK2(.ORKS,ORKDFN,ORKA,ORENT,"SESSION")
 I ORRXDONE=0 D
 .N ORKSMSG,OROITXT
 .S OROITXT=$P($G(^ORD(101.43,OI,0)),U)
 .I $L(OROITXT)>0 S OROITXT=" for "_$$TRIM^XLFSTR(OROITXT)
 .S ORKMSG="Enhanced order checks cannot be done"_OROITXT_". Please perform a manual check for Drug-Interactions, Duplicate Therapy"
 .I $$DS^PSSDSAPI S ORKMSG=ORKMSG_" and Dosing"
 .S ORKS("ORK",2_$E(ORKMSG,1,225))=ORNUM_U_25_U_3_U_ORKMSG
 Q
 ;
RXOCS ;drug-drug interaction, duplicate drug order, duplicate drug class
 Q:ORCRITF_ORSIGF_ORDUPF_ORDUPCF_ORALLRF'["E"  ;quit if none are "E"nabled
 N ORKRX,ORPSNUM,ORY,CHK,XX
 I $L($G(HL7LPTR)),($G(HL7LCOD)="99PSD") D
 .D CHKSESS^ORKPS(.ORKRX,ORKDFN,HL7LPTR_U_HL7LTXT,OI,ORKPDATA,ORKDG,+$P($G(ORPSA),";",4),$P(ORKA,"|",7))
 .S CHK=0,XX="" F  S CHK=$O(ORKRX(CHK)) Q:'CHK  D
 ..S XX=ORKRX(CHK)
 ..;
 ..;get errors/exceptions/checks not done
 ..I $P(XX,U)="ERR" S ORKS("ORK",2_$E($P(XX,U,2),1,225)_","_$G(ORNUM))=ORNUM_U_25_U_3_U_$P(XX,U,2)
 ..;
 ..;critical drug interaction:
 ..I $P(XX,U)="DI",$P(XX,U,2)="CRITICAL" D
 ...Q:ORCRITF="D"
 ...S ORPSNUM=$P(XX,U,3)
 ...S ORKTXT=$P(XX,U,2)_" drug-drug interaction: "_$P(XX,U,5)
 ...S ORKS("ORK",ORCRITD_","_$G(ORNUM)_","_ORPSNUM_","_$E(ORKTXT,1,225))=ORNUM_U_ORCRITN_U_ORCRITD_U_ORKTXT
 ..;
 ..;significant drug interaction:
 ..I $P(XX,U)="DI",$P(XX,U,2)="SIGNIFICANT" D
 ...Q:ORSIGF="D"
 ...S ORPSNUM=$P(XX,U,3)
 ...S ORKTXT=$P(XX,U,2)_" drug-drug interaction: "_$P(XX,U,5)
 ...S ORKS("ORK",ORSIGD_","_$G(ORNUM)_","_ORPSNUM_","_$E(ORKTXT,1,225))=ORNUM_U_ORSIGN_U_ORSIGD_U_ORKTXT
 ..;
 ..;duplicate drug:
 ..I $P(XX,U)="DD" D
 ...Q:ORDUPF="D"
 ...S ORPSNUM=$P(XX,U,4)  ;get the associated order number
 ...S ORKTXT=$P(XX,U,3)
 ...I $L(ORPSNUM),$G(^OR(100,+ORPSNUM,0)) D
 ....D TEXT^ORQ12(.ORY,ORPSNUM,"")
 ....S ORKTXT=ORKTXT_$S($D(ORY(2))=1:" "_$$TRIM^XLFSTR(ORY(2)),1:"")_" ["_$P($G(^ORD(100.01,+$P(^OR(100,+ORPSNUM,3),U,3),0)),U,1)_"]"
 ...S ORKMSG="Duplicate drug order: "_ORKTXT
 ...S ORKS("ORK",ORDUPD_","_$G(ORNUM)_","_ORPSNUM_",Duplicate drug order: "_$E($P(XX,U,3),1,200))=ORNUM_U_ORDUPN_U_ORDUPD_U_ORKMSG_U_$G(ORPSNUM)
 ..;
 ..;duplicate class: NOW DRUG THERAPY
 ..I $P(XX,U)="DC" D
 ...Q:ORDUPCF="D"
 ...S ORPSNUM=$P(XX,U,2)  ;get the associated order number
 ...S ORKMSG=$P(XX,U,4)
 ...S ORKS("ORK",ORDUPCD_","_$G(ORNUM)_","_ORPSNUM_","_$E(ORKMSG,1,225))=ORNUM_U_ORDUPCN_U_ORDUPCD_U_ORKMSG
 D RXOCS^ORKCHK5
 Q
 ;
PARAMS(ORKNAME,ORKNUM,ORKFLAG,ORKDNGR) ; get parameter values for an order chk
 S ORKNUM=0,ORKNUM=$O(^ORD(100.8,"B",ORKNAME,ORKNUM))
 S ORKFLAG=$$GET^XPAR(ORENT,"ORK PROCESSING FLAG",ORKNUM,"I")
 S ORKDNGR=$$GET^XPAR("DIV^SYS^PKG","ORK CLINICAL DANGER LEVEL",ORKNUM,"I")
 Q
