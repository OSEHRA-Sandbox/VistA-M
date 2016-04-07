ORBCMA5 ; SLC/JDL - BCMA Order utility ;07/25/14  14:23
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**133,350**;Dec 17, 1997;Build 77
 ;;BCMA ORDER UTITLITY;**133**;12/12/2001
 ;
 ;
 ;
GETUDID(Y,INFO) ; Get Unit/Dose Order Form ID
 S Y=$O(^ORD(101.41,"B","PSJ OR PAT OE",0))
 S:$L(Y)<1 Y=0
 Q
GETIVID(Y,INFO) ; Get IV Order Form ID
 S Y=$O(^ORD(101.41,"B","PSJI OR PAT FLUID OE",0))
 S:$L(Y)<1 Y=0
 Q
GETUD(Y,ODITM) ; get unit dose orderable item
 ; ODITM: Orderable item ien
 N DGIDX,OIIEN,UDIEN
 S DGIDX=0
 S UDIEN=$O(^ORD(101.44,"B","ORWDSET UD RX",0))
 F  S DGIDX=$O(^ORD(101.44,UDIEN,20,DGIDX)) Q:'DGIDX  D
 . S OIIEN=$P(^ORD(101.44,UDIEN,20,DGIDX,0),U,1)
 . I OIIEN=ODITM S Y=^ORD(101.44,UDIEN,20,DGIDX,0)
 K DGIDX,OIIEN,UDIEN
 Q
ODITMBC(Y,XREF,ODLST) ; --Return orderable items info based on ItemIen
 N CNT,NUM,XRF
 S CNT=0,NUM=0,XRF=""
 S:$L(XREF) XRF=XREF
 F  S CNT=$O(ODLST(CNT)) Q:'CNT  D FNDINFO(.Y,ODLST(CNT))
 Q
FNDINFO(Y,ODIEN) ;
 N ODI,CRTM,FRM,XX,FINDIT
 S XX="",FINDIT=0
 S FRM="",CRTM=$$NOW^XLFDT
 F  S FRM=$O(^ORD(101.43,XRF,FRM)) Q:FRM=""  D
 . S ODI=0 F  S ODI=$O(^ORD(101.43,XRF,FRM,ODI)) Q:'ODI  D
 .. S XX=^ORD(101.43,XRF,FRM,ODI)
 .. I +$P(XX,U,3),$P(XX,U,3)<CRTM Q
 .. I ODI=+ODIEN D
 ... S NUM=NUM+1,FINDIT=1
 ... I 'XX S Y(NUM)=ODIEN_U_$P(XX,U,2)_U_$P(XX,U,2)
 ... E  S Y(NUM)=ODIEN_U_$P(XX,U,2)_$C(9)_"<"_$P(XX,U,4)_">"_U_$P(XX,U,4)
 I FINDIT=0 D
 . S:$D(^ORD(101.43,+ODIEN,0)) XX=^ORD(101.43,+ODIEN,0)
 . S NUM=NUM+1
 . S:$L(XX) Y(NUM)=ODIEN_U_$P(XX,U)_U_$P(XX,U)_U_"NF"
 Q
LOCK(ORY,DFN,ORJOB) ; -- Lock patient chart (silent)
 ; Returns 1 if successful, or 0^Message if could not get lock
 ;
 I '$G(DFN) S ORY="0^Invalid patient" Q
 I $G(ORJOB)="" S ORY="0^Invalid Job" Q
 N Y,ORLK,NOW,NOW1
 S ORLK=$G(^XTMP("ORPTLK-"_DFN,1)) I ORLK=(DUZ_U_ORJOB) S ORY=1 Q  ;locked
 L +^XTMP("ORPTLK-"_DFN):$S($G(DILOCKTM)>0:DILOCKTM,1:5) I '$T S Y="0^"_$S(+ORLK:$P($G(^VA(200,+ORLK,0)),U),1:"Another person")_" is editing orders for this patient." Q Y
 S NOW=$$NOW^XLFDT,NOW1=$$FMADD^XLFDT(NOW,1)
 S ^XTMP("ORPTLK-"_DFN,0)=NOW1_U_NOW_"^CPRS Chart Lock",^(1)=DUZ_U_ORJOB
 S ORY=1 Q
 ;
UNLOCK(ORY,DFN,ORJOB) ; -- Unlock patient chart (silent)
 I '$G(DFN) S ORY=0 Q
 I $G(ORJOB)="" W ORY=0 Q
 L -^XTMP("ORPTLK-"_DFN)
 I $G(^XTMP("ORPTLK-"_DFN,1))=(DUZ_U_ORJOB) K ^XTMP("ORPTLK-"_DFN)
 S ORY=1 Q
 ;
JOB(ORY) ;return $J
 S ORY=$J Q
