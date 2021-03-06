RMPR29W1 ;HOIFO/SPS -  WORK ORDER ADD ON GRID OWL PROGRAM ;11/8/05  07:12
 ;;3.0;PROSTHETICS;**75**;Feb 09, 1996;Build 25
 ;
 ;
A1(RMPR6641) ;entry point for testing
 D A2
 Q
EN(RESULT,RMPR6641) ; -- Broker callback to get list to display
A2 N STRING,CLREND,COLUMN,ON,OFF
 S DATE=2010101
 K ^TMP($J)
 N RMPRA,CDATE,X
 K ADATE,PDAY,RMPRCD
 S (CNT,VALMCNT)=0,RRX=""
 ;S (PPDAY,PPD,PPD1,PPD2,PPD3,PPD4,PPD5)=0
 ;S (PPDDAY,PPDD1,PPDD2,PPDD3,PPDD4,PPDD5)=0
 I '$D(^RMPR(664.1,RMPR6641,0)) Q
 I $P(^RMPR(664.1,RMPR6641,0),U,13)="" Q
 S WO66410=^RMPR(664.1,RMPR6641,0)
 ;W !!,RMPR6641,"  ",WO66410
 S RMPRA=$P(WO66410,U,8) Q:RMPRA'>0  D
 .S STN=$P(^RMPR(668,RMPRA,0),U,7)
 .S STNX=$$STATN^RMPRUTIL(STN)
 .S STS=$P(^RMPR(668,RMPRA,0),U,10)
 .;Q:STS'["P"
 .S DFN=$P(^RMPR(668,RMPRA,0),U,2)
 .D DEM^VADPT
 .S SSNEN=$E($P(VADM(2),"^",2),10,11)
 .K SSNEN,VADM
 .D REC
 G EXIT
 Q
 K CDAY,DNT,DATE,DFN,LINKED,PPD,PPDAY,RMPRAON,RMPRHCSN,RMPRII,RMPRMI
 K RMPRPCE,RMPRPCI,RMPRPHCE,RMPRPHCI,RMPRTC,RMPRTTE,RMPRTTI,RMPRUI
 K RMPRUIE,RRX,STN,STNX,STS,VALMCNT,WO66410,WRKDAY
REC ;records to grid
 ;stop date, init action date
 ;check ien, patch 77
 ;
 ;Q:$D(^TMP($J,RMPRA))
 ;
 N DIC,DIQ,DR,STOPDT
 ;S DA=RMPRA
 ;S DIC=668,DIQ="RE",DR=10,DIQ(0)="EN" D EN^DIQ1
 S STOPDT=$P($G(^RMPR(668,RMPRA,0)),U,9),STOPDT=$$DAT2^RMPRUTL1(STOPDT)
 S CDATE=$P(^RMPR(668,RMPRA,0),U,1),CDATE=$$DAT2^RMPRUTL1(CDATE)
 S DFN=$P(^RMPR(668,RMPRA,0),U,2) Q:DFN=""
 N VA,VADM
 D DEM^VADPT
 S WHO=VADM(1)
 S SSN=VADM(2)
 D KVAR^VADPT
 ;type
 S TYPE=$$TYPE^RMPREOU(RMPRA,8)
 Q:TYPE'["LAB"
 ;W !,"AFTER",RMPRA,"   ",WO66410
 ;display description if manual
 S DES=$$DES^RMPREOU(RMPRA,22)
 S DES=$TR(DES,"^","*")
 S DES=$TR(DES,"""","'")
 ;init action date
 S ADATE="",PDAY="",WRKDAY=""
 S ADATE=$P(^RMPR(668,RMPRA,0),U,9)
 ;PPD=1 for previous pending
 I ADATE'="" S CDAY=$$PDAY^RMPREOU(RMPRA)
 ;
 S STATUS=$$STATUS^RMPREOU(RMPRA)
 I STATUS["PENDING" D
 .I ADATE'=""&(ADATE<DATE) S PPD=1
 .S PPDAY=$$PWRKDAY^RMPREOU(RMPRA)
 S LINKED=$P($G(^RMPR(668,RMPRA,10,0)),U,4)
 I LINKED="" S LINKED=0
 ;
 ;Get Work Order Data and add to array
 S RMPRAON="" F  S RMPRAON=$O(^RMPR(664.1,RMPR6641,2,RMPRAON)) Q:RMPRAON=""  D
 . Q:'$D(^RMPR(664.1,RMPR6641,2,RMPRAON,0))
 . ;Change to send only item 1
 . ;Q:RMPRAON>1
 . S CNT=CNT+1
 . S RMPRII=^RMPR(664.1,RMPR6641,2,RMPRAON,0)
 . ;S RMPR6642=$P(RMPRII,U,6)
 . ;Internal and external item
 . ;S RMPRSOI=$P(RMPRII,U),RMPRSOIE=$$EXTERNAL^DILFD(664.16,.01,"",RMPRSOI)
 . ;Chose to display 661.1 short description instead. left for PFU.
 . ;Internal/external unit of issue
 . S RMPRUI=$P(RMPRII,U,3),RMPRUIE=$$EXTERNAL^DILFD(664.16,3,"",RMPRUI)
 . ;Internal/external Type of Transaction
 . S RMPRTTI=$P(RMPRII,U,7),RMPRTTE=$$EXTERNAL^DILFD(664.16,8,"",RMPRTTI)
 . ;Internal/external patient category
 . S RMPRPCI=$P(RMPRII,U,8),RMPRPCE=$$EXTERNAL^DILFD(664.16,9,"",RMPRPCI)
 . ;Internal/External HCPCS
 . S RMPRPHCI=$P($G(^RMPR(664.1,RMPR6641,2,RMPRAON,2)),U,1),RMPRPHCE=$$EXTERNAL^DILFD(661.1,1,"",RMPRPHCI)
 . ;HCPCS SHORT NAME
 . I +RMPRPHCI>0 S RMPRHCSN=$P($G(^RMPR(661.1,RMPRPHCI,0)),U,2)
 . E  S RMPRHCSN="UNKNOWN HCPCS NAME"
 . ;Internal/External CPT Modifier
 . S RMPRMI=$P($G(^RMPR(664.1,RMPR6641,2,RMPRAON,2)),U,2)
 . S RMPRTC=$P(RMPRII,U,11)
 . S ^TMP($J,RMPRA,CNT)=0_U_RMPRAON_U_RMPRHCSN_U_$P(RMPRII,U,2)_U_RMPRUIE_U_$P(RMPRII,U,4)_U_$P(RMPRII,U,5)
 . S ^TMP($J,RMPRA,CNT)=^TMP($J,RMPRA,CNT)_U_$P(RMPRII,U,6)_U_RMPRTTE_U_RMPRPCE
 . S ^TMP($J,RMPRA,CNT)=^TMP($J,RMPRA,CNT)_U_RMPRPHCE_U_RMPRMI_U_RMPRTC
 K CDATE,WHO,SSN,TYPE,DES,PDAY,STATUS,ADATE
 ;PUT RESULTS IN GLOBAL!!
 Q
EXIT ;common exit point
 S RESULT=$NA(^TMP($J))
 Q
