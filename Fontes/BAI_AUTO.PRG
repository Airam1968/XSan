*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa	:
* Data        : 01/01/1999
* Objetivo    : BAIXA AUTOM‡TICA DE LAN‡AMENTOS BANCARIOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION BAI_AUTO()
SAVE SCREEN TO TELA04
PLQUA("11")
PTIT( "BAIXA AUTOMµTICA DE LAN€AMENTOS BANCµRIOS")
SELE 1
USE STATUS
GOTO TOP
eBAIXA_ATIVO = BAIXA_ATIV
sisCGC_CPF=CGC_CPF                         && CGC/CPF da Empresa
PLQUA("01")
PMSGR( "Aten‡Æo !",3)
PMENSAG("Esta Opera‡Æo NÆo ‚ Revers¡vel !!!, Tecle Enter ...")
PPARA()
PLQUA("01")
PPERG ( "Confirma a Baixa Autom tica (S/N) ? ")
IF sisRESP = "N"
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA04
   RETURN nil
ENDIF
PLQUA("01")
PMENSAG("Filtrando as Baixas h  Serem Realizadas, Aguarde.")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
GOTO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 3
USE CAD_QUAD INDEX CAD_QUAD
GOTO TOP
SELE 4
USE CAD_APAR INDEX CAD_APAR
GOTO TOP
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SELE 6
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 8
* Cadastro de Parcelas
SELE 9
USE INDICES INDEX INDICES
GOTO TOP
SELE 8
USE BANCOS INDEX BANCOS
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
eCOD_BANCO = 0
@ 05,02 SAY "Banco   : "
GO TOP
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="@R9999"
PIC[2]="@S50"
CAB[1]="    "
CAB[2]="    "
DBEDIT(5,15,5,75,CAMPO,"F_CONF_COB",PIC,CAB," "," - "," ")
eCOD_BANCO = COD_BANCO
eNOME_ARQ = ALLTRIM(NOME_ARQ)
eLOCALIZA = .T.
ePESQ1    = 1
SET EXCLUSIVE ON
DO CASE
   CASE eCOD_BANCO = 1
      SELE 7
      USE TRANS001
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06"
      GO TOP
   CASE eCOD_BANCO = 8
      SELE 7
      USE TRANS008
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06"
      GO TOP
   CASE eCOD_BANCO = 33
      SELE 7
      USE TRANS033
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "15"
      GO TOP
   CASE eCOD_BANCO = 104
      SELE 7
      USE TRANS104
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "21" .OR. CDOC_TRN01 = "01" .OR. CDOC_TRN01 = "22"
      GO TOP
   CASE eCOD_BANCO = 215
      SELE 7
      USE TRANS215
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 230
      SELE 7
      USE TRANS230
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 237
      SELE 7
      USE TRANS237
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 275
      SELE 7
      USE TRANS275
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 291
      SELE 7
      USE TRANS291
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "08"
      GO TOP
   CASE eCOD_BANCO = 341
      SELE 7
      USE TRANS341
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "07" .OR. CDOC_TRN01 = "08" 
      GO TOP
   CASE eCOD_BANCO = 409
      SELE 7
      USE TRANS409
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "98" .OR. CDOC_TRN01 = "99" 
      GO TOP
   CASE eCOD_BANCO = 422
      SELE 7
      USE TRANS422
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "07" .OR. CDOC_TRN01 = "15"
      GO TOP
   CASE eCOD_BANCO = 424
      SELE 7
      USE TRANS424
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "15"
      GO TOP
   CASE eCOD_BANCO = 641
      SELE 7
      USE TRANS641
      GOTO TOP
      SET FILTER TO CDOC_TRN01 = "02" .OR. CDOC_TRN01 = "06" .OR. CDOC_TRN01 = "15" .OR. CDOC_TRN01 = "16" .OR. CDOC_TRN01 = "17" 
      GO TOP
   OTHERWISE
      SELE 7
      USE TRANSTXT
      GOTO TOP
ENDCASE
SET EXCLUSIVE OFF
IF EOF()
   PLQUA("01")
   PMSGR( "Aten‡Æo !",3)
   PMENSAG("NÆo H  Baixas h  Serem Realizadas !")
   PPARA()
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA04
   RETURN nil
ELSE
   SELE 7
   GOTO TOP
   DO WHILE .NOT. EOF()
      SELE 7
      * Identifica‡Æo do Titulo na Empresa
      eIDENTIF = ALLTRIM(UEMP_TRN01)
      * Identifica‡Æo do Codigo do Boleto
      DO CASE
         CASE eCOD_BANCO = 1
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,11,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 10,REPLICATE("0",10-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,10,0)),ALLTRIM(STR(NONU_TRN01,10,0)))
         CASE eCOD_BANCO = 8
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,11,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 10,REPLICATE("0",10-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,10,0)),ALLTRIM(STR(NONU_TRN01,10,0)))
         CASE eCOD_BANCO = 33
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,11,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 10,REPLICATE("0",10-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,10,0)),ALLTRIM(STR(NONU_TRN01,10,0)))
         CASE eCOD_BANCO = 104
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,12,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 12,REPLICATE("0",12-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,12,0)),ALLTRIM(STR(NONU_TRN01,12,0)))
         CASE eCOD_BANCO = 215
            eTAM_BOL = LEN(ALLTRIM(FIL2_TRN01))
            eCOD_BOL = IIF(eTAM_BOL <> 11,REPLICATE("0",11-eTAM_BOL)+ALLTRIM(FIL2_TRN01),ALLTRIM(FIL2_TRN01))
         CASE eCOD_BANCO = 230
            eTAM_BOL = LEN(ALLTRIM(FIL2_TRN01))
            eCOD_BOL = IIF(eTAM_BOL <> 11,REPLICATE("0",11-eTAM_BOL)+ALLTRIM(FIL2_TRN01),ALLTRIM(FIL2_TRN01))
         CASE eCOD_BANCO = 237
            eTAM_BOL = LEN(ALLTRIM(FIL2_TRN01))
            eCOD_BOL = IIF(eTAM_BOL <> 11,REPLICATE("0",11-eTAM_BOL)+ALLTRIM(FIL2_TRN01),ALLTRIM(FIL2_TRN01))
         CASE eCOD_BANCO = 275
            eTAM_BOL = LEN(ALLTRIM(FIL2_TRN01))
            eCOD_BOL = IIF(eTAM_BOL <> 11,REPLICATE("0",11-eTAM_BOL)+ALLTRIM(FIL2_TRN01),ALLTRIM(FIL2_TRN01))
         CASE eCOD_BANCO = 291
            eTAM_BOL = LEN(ALLTRIM(FIL2_TRN01))
            eCOD_BOL = IIF(eTAM_BOL <> 11,REPLICATE("0",11-eTAM_BOL)+ALLTRIM(FIL2_TRN01),ALLTRIM(FIL2_TRN01))
         CASE eCOD_BANCO = 341
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,8,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 8,REPLICATE("0",8-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,8,0)),ALLTRIM(STR(NONU_TRN01,8,0)))
         CASE eCOD_BANCO = 409
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,10,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 10,REPLICATE("0",10-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,10,0)),ALLTRIM(STR(NONU_TRN01,10,0)))
         CASE eCOD_BANCO = 422
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,10,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 10,REPLICATE("0",10-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,10,0)),ALLTRIM(STR(NONU_TRN01,10,0)))
         CASE eCOD_BANCO = 424
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,10,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 10,REPLICATE("0",10-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,10,0)),ALLTRIM(STR(NONU_TRN01,10,0)))
         CASE eCOD_BANCO = 641
            eTAM_BOL = LEN(ALLTRIM(STR(NONU_TRN01,15,0)))
            eCOD_BOL = IIF(eTAM_BOL <> 15,REPLICATE("0",15-eTAM_BOL)+ALLTRIM(STR(NONU_TRN01,15,0)),ALLTRIM(STR(NONU_TRN01,15,0)))
      ENDCASE
      * Identifica‡ao do C¢digo da Carteira
      eCOD_CAR = SUBSTR(IOBA_TRN01,1,3)
      * Data do Cr‚dito
      eTAM_DTC = LEN(ALLTRIM(STR(DTCR_TRN01,6,0)))
      eDT_CRED = IIF(eTAM_DTC <> 6,REPLICATE("0",6-eTAM_DTC)+ALLTRIM(STR(DTCR_TRN01,6,0)),ALLTRIM(STR(DTCR_TRN01,6,0)))
      eANO     = 0
      eANO     = VAL(SUBSTR(eDT_CRED,5,2))
      IF eANO >= 50 .AND. eANO <= 99
         eDT_CRED = CTOD(SUBSTR(eDT_CRED,1,2)+"/"+SUBSTR(eDT_CRED,3,2)+"/"+"19"+SUBSTR(eDT_CRED,5,2))
      ELSE
         eDT_CRED = CTOD(SUBSTR(eDT_CRED,1,2)+"/"+SUBSTR(eDT_CRED,3,2)+"/"+"20"+SUBSTR(eDT_CRED,5,2))
      ENDIF
      * Data do Vencimento
      eTAM_DTV = LEN(ALLTRIM(STR(DTVE_TRN01,6,0)))
      eDT_VENC = IIF(eTAM_DTV <> 6,REPLICATE("0",6-eTAM_DTV)+ALLTRIM(STR(DTVE_TRN01,6,0)),ALLTRIM(STR(DTVE_TRN01,6,0)))
      eANO     = 0
      eANO     = VAL(SUBSTR(eDT_VENC,5,2))
      IF eANO >= 50 .AND. eANO <= 99
         eDT_VENC = CTOD(SUBSTR(eDT_VENC,1,2)+"/"+SUBSTR(eDT_VENC,3,2)+"/"+"19"+SUBSTR(eDT_VENC,5,2))
      ELSE
         eDT_VENC = CTOD(SUBSTR(eDT_VENC,1,2)+"/"+SUBSTR(eDT_VENC,3,2)+"/"+"20"+SUBSTR(eDT_VENC,5,2))
      ENDIF
      * Valor Original do T¡tulo
      eVLR_ORI = VLTI_TRN01
      * Valor Principal do T¡tulo, Descontado o Custo da Cobran‡a
      eVLR_PRI = VPRI_TRN01
      eVLR_JUM = JMMU_TRN01
      eVLR_TOT = eVLR_ORI + eVLR_JUM 
      * Numero Sequencial do Lan‡amento Banc rio
      eNUM_SEQ = NSEQ_TRN01
      eCOD_SEQ = CDOC_TRN01
      ***
      eNUM_PARC= SPACE(2)
      * Desmenbramento da Identifica‡Æo
      IF eIDENTIF <> SPACE(8) .OR. eCOD_BANCO = 341
         IF ePESQ1 = 1
            IF eCOD_BANCO = 341  && PARA NET EMPREENDIM
               eEMPREEN  = VAL(SUBSTR(eCOD_BOL,1,2))
               eQUADRA   = VAL(SUBSTR(eCOD_BOL,3,2))
               eIMOVEL   = VAL(SUBSTR(eCOD_BOL,5,2))
               ePARTE    = 0
               eNUM_PARC = VAL(SUBSTR(eCOD_BOL,7,2))
            ELSE
               eEMPREEN  = VAL(SUBSTR(eIDENTIF,1,2))
               eQUADRA   = VAL(SUBSTR(eIDENTIF,3,2))
               eIMOVEL   = VAL(SUBSTR(eIDENTIF,5,3))
               ePARTE    = VAL(SUBSTR(eIDENTIF,8,1))
               IF sisCGC_CPF = "02852392000142" .OR. sisCGC_CPF = "06983112000196"
                  ePARTE    = 0
               ENDIF
            ENDIF
         ELSE
            * ALTERACAO PARA O SISTEMA DE NUMERO DE PARCELAS NO NOSSO NUMERO
            IF eCOD_BANCO = 341  && PARA NET EMPREENDIM
               eEMPREEN  = VAL(SUBSTR(eCOD_BOL,1,2))
               eQUADRA   = VAL(SUBSTR(eCOD_BOL,3,2))
               eIMOVEL   = VAL(SUBSTR(eCOD_BOL,5,2))
               ePARTE    = 0
               eNUM_PARC = VAL(SUBSTR(eCOD_BOL,7,2))
            ELSE
               eEMPREEN  = VAL(SUBSTR(eIDENTIF,1,2))
               eQUADRA   = VAL(SUBSTR(eIDENTIF,3,2))
               eIMOVEL   = VAL(SUBSTR(eIDENTIF,5,4))
               ePARTE    = 0
            ENDIF
         ENDIF
         @ 06,00 CLEAR TO 23,79
	 PCENTRA( "INFORMA€åES DO IMàVEL",6)
         SELE 2
         GOTO TOP
         SEEK eEMPREEN
         @ 07,02 SAY "   Produto     : "+STR(eEMPREEN,5)+" - "+ALLTRIM(DESCRICAO)
         eTIPO  = TIPO
         eCONTA = COD_CONTA
         IF eTIPO = 1
            SELE 3
            GOTO TOP
            SEEK STR(eEMPREEN,5)+STR(eQUADRA,5)
            @ 08,02 SAY sisGRUPO1+"         : "+STR(eQUADRA,5)
         ELSE
            SELE 4
            GOTO TOP
            SEEK STR(eEMPREEN,5)+STR(eQUADRA,5)
            @ 08,02 SAY sisGRUPO2+"         : "+STR(eQUADRA,5)
         ENDIF
         SELE 6
         GOTO TOP
         SEEK STR(eEMPREEN,5)+STR(eQUADRA,5)+STR(eIMOVEL,5)+STR(ePARTE,1)
         ePARTE_T = IIF(ePARTE = 1 .OR. ePARTE = 0,"1o. Propriet rio","2o. Propriet rio")
         @ 09,02 SAY sisUNID1+"         : "+STR(eIMOVEL,5)+" - "+STR(ePARTE,1)+" - "+ePARTE_T
	 PCENTRA( "INFORMA€åES DO PAGAMENTO",10)
         SELE 8
         eIND_CLAS = STR(eEMPREEN,5)
         eIND_QUAD = STR(eQUADRA,5)
         eZERO = "000"
         eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)+".DBF"
         eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
         IF FILE(eARQ)
            USE &eARQ INDEX &eIND1,&eIND2,&eIND3
            GOTO TOP
         ELSE
            SELE 7
            SKIP
            LOOP
         ENDIF
         DO WHILE .T.
         IF (eBAIXA_ATIVO = .T.) &&BAIXA PARCELAS ATIVAS E INATIVAS
            DO CASE
               CASE eCOD_BANCO = 1
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 8
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 33
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 104
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 215
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 230
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 237
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = 0 .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 275
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 291
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = 0 .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 341
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. NUM_PARC = STR(eNUM_PARC,5)
                  ** && DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 409
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 422
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 424
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 641
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC
                  GOTO TOP
            ENDCASE
         ELSE
	&&BAIXA PARCELAS ATIVAS
            DO CASE
               CASE eCOD_BANCO = 1
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 8
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 33
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 104
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 215
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 230
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 237
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = 0 .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 275
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 291
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = 0 .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 341
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. NUM_PARC = STR(eNUM_PARC,5) .AND. ATIVA = .T.
                  ** && DATA_VENC = eDT_VENC
                  GOTO TOP
               CASE eCOD_BANCO = 409
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 422
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 424
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
               CASE eCOD_BANCO = 641
                  SET FILTER TO CLASSE = STR(eEMPREEN,5) .AND. QUADRA = STR(eQUADRA,5) .AND. NUM_IMOVEL = STR(eIMOVEL,5) .AND. PARTE = ePARTE .AND. DATA_VENC = eDT_VENC .AND. ATIVA = .T.
                  GOTO TOP
            ENDCASE         
         END
            EXIT
         ENDDO
         @ 11,10 TO 22,65
         @ 12,30 TO 21,30
         @ 12,48 TO 21,48
         * Texto
         @ 12,12 SAY "T¡tulos"
         @ 12,32 SAY "Inf. Parcela"
         @ 12,50 SAY "Inf. Pagto."
         @ 14,12 SAY "Num. Parcela"        
         @ 15,12 SAY "Vencimento"
         @ 16,12 SAY "Vlr. Parcela"
         @ 17,12 SAY "Pagamento"
         @ 18,12 SAY "Vlr. Pago"
         @ 19,12 SAY "C¢digo Boleto"
         @ 20,12 SAY "C¢digo Carteira"
         @ 21,12 SAY "Sequencial"
         * Valores da Parcela
         @ 14,37 GET NUM_PARC  PICT "99999"
         @ 15,32 GET DATA_VENC   
         @ 16,32 GET VLR_PARC  PICT "@R 999,999.99"
         @ 19,32 GET COD_BANCO PICT "@R 9999999999999"
         * Valores do Pagamento
         @ 15,50 GET eDT_VENC
         @ 16,50 GET eVLR_ORI  PICT "@R 999,999.99"
         @ 17,50 GET eDT_CRED            
         @ 18,50 GET eVLR_TOT  PICT "@R 999,999.99"
         @ 19,50 GET eCOD_BOL 
         @ 20,57 GET eCOD_CAR  PICT "@R 999"
         @ 21,54 GET eNUM_SEQ  PICT "@R 999999"
         CLEAR GETS

******
         IF (sisCGC_CPF <> "62107453000179") .or. (sisCGC_CPF <> "01723214000159")
            PPARA()
         END

         IF EOF()
            PLQUA("01")
            PMENSAG("Parcela No. "+ALLTRIM(eIDENTIF)+" NÆo Localizada !")
            IF ePESQ1 = 1
               eIMOVEL = eIMOVEL+ePARTE
               ePARTE  = 0
               ePESQ1  = ePESQ1 + 1
               SELE 7
               LOOP
            ELSE
               ePESQ1 = 1
               SELE 7
               SKIP
               LOOP
            ENDIF
         ELSE
            ePESQ1 = 1
            DO WHILE .NOT. EOF()
               @ 11,10 TO 22,65
               @ 12,30 TO 21,30
               @ 12,48 TO 21,48
               * Texto
               @ 12,12 SAY "T¡tulos"
               @ 12,32 SAY "Inf. Parcela"
               @ 12,50 SAY "Inf. Pagto."
               @ 14,12 SAY "Num. Parcela"        
               @ 15,12 SAY "Vencimento"
               @ 16,12 SAY "Vlr. Parcela"
               @ 17,12 SAY "Pagamento"
               @ 18,12 SAY "Vlr. Pago"
               @ 19,12 SAY "C¢digo Boleto"
               @ 20,12 SAY "C¢digo Carteira"
               @ 21,12 SAY "Sequencial"
               * Valores da Parcela
               @ 14,37 GET NUM_PARC  PICT "99999"
               @ 15,32 GET DATA_VENC   
               @ 16,32 GET VLR_PARC  PICT "@R 999,999.99"
               @ 19,32 GET COD_BANCO PICT "@R 9999999999999"
               * Valores do Pagamento
               @ 15,50 GET eDT_VENC
               @ 16,50 GET eVLR_ORI  PICT "@R 999,999.99"
               @ 17,50 GET eDT_CRED            
               @ 18,50 GET eVLR_TOT  PICT "@R 999,999.99"
               @ 19,50 GET eCOD_BOL 
               @ 20,57 GET eCOD_CAR  PICT "@R 999"
               @ 21,54 GET eNUM_SEQ  PICT "@R 999999"
               CLEAR GETS


               IF (sisCGC_CPF <> "62107453000179") .or. (sisCGC_CPF <> "01723214000159")
                  PPARA()
               END

               IF DATA_VENC <> eDT_VENC .OR. (ALLTRIM(eCOD_BOL) <> ALLTRIM(COD_BANCO) .AND. COD_BANCO <> SPACE(10)) && .OR. INT(VLR_PARC) <> INT(eVLR_ORI) 
                  IF INT(VLR_PARC) <> INT(eVLR_ORI)
                     SELE 8
                     SKIP
                     LOOP
                  ENDIF
                  PMSGR( "Aten‡Æo !",3)
                  PLQUA("01")
                  PPERG ( "Confirma a Baixa (S/N) ? ")
                  IF sisRESP = "S"
                     IF F_BLOQR()
                        DO CASE
                           CASE eCOD_BANCO = 1
                              IF eCOD_SEQ = "06"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 8
                              IF eCOD_SEQ = "06"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 33
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "15"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 104 
                              IF eCOD_SEQ = "21" .OR. eCOD_SEQ = "22"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 215
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 230
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 237
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 275
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 291
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 341
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 409
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "98" .OR. eCOD_SEQ = "99"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                               ENDIF
                           CASE eCOD_BANCO = 422
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "07" .OR. eCOD_SEQ = "09" .OR. eCOD_SEQ = "15"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                           CASE eCOD_BANCO = 424
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "15"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                               ENDIF
                           CASE eCOD_BANCO = 641
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "15" .OR. eCOD_SEQ = "16"  .OR. eCOD_SEQ = "17" .OR. eCOD_SEQ = "09"  
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                        ENDCASE
                        REPLACE COD_BANCO WITH eCOD_BOL
                        UNLOCK
                     ENDIF
                  ENDIF
               ELSE
                  IF F_BLOQR()
                     DO CASE
                        CASE eCOD_BANCO = 1
                           IF eCOD_SEQ = "06"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 8
                           IF eCOD_SEQ = "06"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 33
                           IF eCOD_SEQ = "06".OR. eCOD_SEQ = "15"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 104
                           IF eCOD_SEQ = "21" .OR. eCOD_SEQ = "22"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 215
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 230
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 237
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 275
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 291
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 341
                              IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "08"
                                 REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                         DATA_PGTO WITH eDT_CRED
                                 REPLACE CONTA     WITH eCONTA
                              ENDIF
                        CASE eCOD_BANCO = 409
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "98" .OR. eCOD_SEQ = "99"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                            ENDIF
                        CASE eCOD_BANCO = 422
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "07" .OR. eCOD_SEQ = "09" .OR. eCOD_SEQ = "15"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 424
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "15"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                        CASE eCOD_BANCO = 641
                           IF eCOD_SEQ = "06" .OR. eCOD_SEQ = "15" .OR. eCOD_SEQ = "16" .OR. eCOD_SEQ = "17" .OR. eCOD_SEQ = "09"
                              REPLACE VLR_PGTO  WITH eVLR_TOT,;
                                      DATA_PGTO WITH eDT_CRED
                              REPLACE CONTA     WITH eCONTA
                           ENDIF
                     ENDCASE
                     REPLACE COD_BANCO WITH eCOD_BOL
                     UNLOCK
                  ENDIF
               ENDIF
               SELE 8
               SKIP
            ENDDO
         ENDIF
      ENDIF
      SELE 7
      SKIP
   ENDDO
   PLQUA("01")
   PMSGR( "Baixa Concluida !",3)
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
