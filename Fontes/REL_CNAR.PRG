*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CNAR.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO COBRANCA BANCARIA - ARQUIVO RETORNO - ERROS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_CNAR()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO COBRANCA BANCARIA - ERROS")
   PLQUA("01")
   eTIPO = SPACE(1)
   @ 23,02 SAY "Ordem Alfab‚tica ou Num‚rica (A/N)  : " GET eTIPO PICT "X" VALID eTIPO $("AN")
   READ
   eARQUIVO = SPACE(60)
   eARQUIVO = sisDRIVE_REL+":\"+sisPASTA_REL+"CTAPAG.TXT"+SPACE(30)
   IF (ALLTRIM(sisDRIVE_REL) = "").and.(ALLTRIM(sisPASTA_REL) = "") 
      eARQUIVO = "C:\RELATORIOS\RELATORIO.TXT          "
   ENDIF        
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SELE 2
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
   eNOME_ARQ  = ALLTRIM(NOME_ARQ)
   SET EXCLUSIVE ON
   DO CASE
      CASE eCOD_BANCO = 1
         SELE 1
         USE TRANS001
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06"
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 8
         SELE 1
         USE TRANS008
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06"
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 33
         SELE 1
         USE TRANS033
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "15"
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 104
         SELE 1
         USE TRANS104
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "01" .AND. CDOC_TRN01 <> "21"
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 215
         SELE 1
         USE TRANS215
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "07" .AND. CDOC_TRN01 <> "08" 
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 230
         SELE 1
         USE TRANS230
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "07" .AND. CDOC_TRN01 <> "08" 
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 237
         SELE 1
         USE TRANS237
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "07" .AND. CDOC_TRN01 <> "08" 
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 275
         SELE 1
         USE TRANS275
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "08" 
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 291
         SELE 1
         USE TRANS291
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "07" .AND. CDOC_TRN01 <> "08" 
         GO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 341
         SELE 1
         USE TRANS341
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06"
         GO TOP
         IF eTIPO = "A"
            INDEX ON NOMS_TRN01 TO TRANSTXT
            GO TOP
         ELSE
            INDEX ON UEMP_TRN01 TO TRANSTXT
            GO TOP
         ENDIF
      CASE eCOD_BANCO = 409
         SELE 1
         USE TRANS409
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "98" .AND. CDOC_TRN01 <> "99"
         GO TOP
         IF eTIPO = "A"
            INDEX ON NOMS_TRN01 TO TRANSTXT
            GO TOP
         ELSE
            INDEX ON UEMP_TRN01 TO TRANSTXT
            GO TOP
         ENDIF
      CASE eCOD_BANCO = 422
         SELE 1
         USE TRANS422
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "07" .AND. CDOC_TRN01 <> "10"
         GO TOP
         IF eTIPO = "A"
            INDEX ON NOMS_TRN01 TO TRANSTXT
            GO TOP
         ELSE
            INDEX ON UEMP_TRN01 TO TRANSTXT
            GO TOP
         ENDIF
      CASE eCOD_BANCO = 424
         SELE 1
         USE TRANS424
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "15"
         GO TOP
         IF eTIPO = "A"
            INDEX ON NOMS_TRN01 TO TRANSTXT
            GO TOP
         ELSE
            INDEX ON UEMP_TRN01 TO TRANSTXT
            GO TOP
         ENDIF
      CASE eCOD_BANCO = 641
         SELE 1
         USE TRANS641
         GOTO TOP
         SET FILTER TO CDOC_TRN01 <> "02" .AND. CDOC_TRN01 <> "06" .AND. CDOC_TRN01 <> "15" .AND. CDOC_TRN01 <> "16" .AND. CDOC_TRN01 <> "17" 
         GO TOP
         IF eTIPO = "A"
            INDEX ON NOMS_TRN01 TO TRANSTXT
            GO TOP
         ELSE
            INDEX ON UEMP_TRN01 TO TRANSTXT
            GO TOP
         ENDIF
      OTHERWISE
         SELE 1
         USE TRANSTXT
         GOTO TOP
   ENDCASE
   SET EXCLUSIVE OFF
   eTOTAL   = 0
   eTOT_CUS = 0
   SET DEVICE TO PRINT
   DO WHILE .NOT. EOF()
      @ 01,00 SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,115 SAY "Data : "+dtoc(DATE())
      @ 02,00 SAY "Sistema : "+sisTIT2
      @ 02,115 SAY "Relat: SAN001"
      @ 03,30 SAY sisLETRAE+"RELATORIO COBRANCA BANCARIA - ERROS - "+eNOME_ARQ+sisLETRAC
      @ 04,00 SAY REPLICATE("=",135)
      @ 05,00  SAY "|"
      @ 05,02  SAY "NOSSO NUM."
      @ 05,14  SAY "NOME CLIENTE"
      @ 05,46  SAY "DATA VENC."
      @ 05,60  SAY "VLR. TITULO"
      @ 05,90  SAY "OCORR."
      @ 05,100 SAY "ERRO"
      @ 05,108 SAY "NUM. LANC."
      @ 05,122 SAY "CUSTO"
      @ 05,134 SAY "|"
      @ 06,00 SAY REPLICATE("=",135)
      LIN = 7
      DO WHILE .NOT. EOF() .AND. LIN <= 54
         * Data do Cr‡dito
         eTAM_DTC = LEN(ALLTRIM(STR(DTCR_TRN01,6,0)))
         eDT_CRED = IIF(eTAM_DTC <> 6,REPLICATE("0",6-eTAM_DTC)+ALLTRIM(STR(DTCR_TRN01,6,0)),ALLTRIM(STR(DTCR_TRN01,6,0)))
         eANO     = 0
         eANO     = VAL(SUBSTR(eDT_CRED,5,2))
         IF eANO >= 50 .AND. eANO <= 99
            eDATA_PGTO = CTOD(SUBSTR(eDT_CRED,1,2)+"/"+SUBSTR(eDT_CRED,3,2)+"/"+"19"+SUBSTR(eDT_CRED,5,2))
         ELSE
            eDATA_PGTO = CTOD(SUBSTR(eDT_CRED,1,2)+"/"+SUBSTR(eDT_CRED,3,2)+"/"+"20"+SUBSTR(eDT_CRED,5,2))
         ENDIF
         * Data do Vencimento
         eTAM_DTV = LEN(ALLTRIM(STR(DTVE_TRN01,6,0)))
         eDT_VENC = IIF(eTAM_DTV <> 6,REPLICATE("0",6-eTAM_DTV)+ALLTRIM(STR(DTVE_TRN01,6,0)),ALLTRIM(STR(DTVE_TRN01,6,0)))
         eANO     = 0
         eANO     = VAL(SUBSTR(eDT_VENC,5,2))
         IF eANO >= 50 .AND. eANO <= 99
            eDATA_VENC = CTOD(SUBSTR(eDT_VENC,1,2)+"/"+SUBSTR(eDT_VENC,3,2)+"/"+"19"+SUBSTR(eDT_VENC,5,2))
         ELSE
            eDATA_VENC = CTOD(SUBSTR(eDT_VENC,1,2)+"/"+SUBSTR(eDT_VENC,3,2)+"/"+"20"+SUBSTR(eDT_VENC,5,2))
         ENDIF
         @ LIN,00  SAY "|"
         @ LIN,02  SAY ALLTRIM(UEMP_TRN01)
         IF eCOD_BANCO = 341
            @ LIN,14  SAY ALLTRIM(NOMS_TRN01)
         ENDIF
         @ LIN,46  SAY eDATA_VENC
         @ LIN,60  SAY VLTI_TRN01 PICT "@R 999,999,999,999.99"
         @ LIN,90  SAY CDOC_TRN01 PICT "@R 99"
         IF eCOD_BANCO = 341
            @ LIN,100 SAY ERRO_TRN01 PICT "@R 99"
         ENDIF
         @ LIN,108 SAY NSEQ_TRN01 PICT "@R 99999"
         @ LIN,120 SAY TACO_TRN01 PICT "@R 999,999.99"         
         @ LIN,134 SAY "|"
         LIN = LIN + 1
         eTOTAL   = eTOTAL + VLTI_TRN01
         IF eCOD_BANCO <> 001
            eTOT_CUS = eTOT_CUS + TACO_TRN01
         ENDIF
         SKIP
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
      IF EOF()
         @ LIN,00 SAY REPLICATE("-",135)
         LIN = LIN + 1
         @ LIN,02  SAY "Total Geral : "
         @ LIN,60  SAY eTOTAL   PICT "@R 999,999,999,999.99" 
         @ LIN,120 SAY eTOT_CUS PICT "@R 999,999.99"         
      ENDIF
      @ 56,00 SAY REPLICATE("=",135)
      @ 57,95 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************
