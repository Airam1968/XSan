*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CNAG.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE ARQUIVO RETORNO - GERAL
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_CNAG()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO COBRAN€A BANCµRIA - ARQUIVO RETORNO - GERAL")
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
   eNOME_ARQ = ALLTRIM(NOME_ARQ)
   SET EXCLUSIVE ON
   DO CASE
      CASE eCOD_BANCO = 1
         SELE 1
         USE TRANS001
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 8
         SELE 1
         USE TRANS008
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 33
         SELE 1
         USE TRANS033
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 104
         SELE 1
         USE TRANS104
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 215
         SELE 1
         USE TRANS215
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 230
         SELE 1
         USE TRANS230
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 237
         SELE 1
         USE TRANS237
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 275
         SELE 1
         USE TRANS275
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 291
         SELE 1
         USE TRANS291
         GOTO TOP
         INDEX ON UEMP_TRN01 TO TRANSTXT
         GO TOP
      CASE eCOD_BANCO = 341
         SELE 1
         USE TRANS341
         GOTO TOP
         IF eTIPO = "A"
            SELE 1
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
         IF eTIPO = "A"
            SELE 1
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
         IF eTIPO = "A"
            SELE 1
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
         IF eTIPO = "A"
            SELE 1
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
         IF eTIPO = "A"
            SELE 1
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
   eTOT_TIT = 0
   eTOT_CRE = 0
   SET DEVICE TO PRINT
   DO WHILE .NOT. EOF()
      @ 01,00 SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,115 SAY "Data : "+dtoc(DATE())
      @ 02,00 SAY "Sistema : "+sisTIT2
      @ 02,115 SAY "Relat: SAN001"
      @ 03,30 SAY sisLETRAE+"COBRANCA BANCARIA - GERAL - "+eNOME_ARQ+sisLETRAC
      @ 04,00 SAY REPLICATE("=",135)
      @ 05,00  SAY "|"
      @ 05,02  SAY "NOSSO NUM."
      @ 05,14  SAY "NOME CLIENTE"
      @ 05,46  SAY "DATA VENC."
      @ 05,58  SAY "VLR. TITULO"
      @ 05,80  SAY "DATA PGTO."
      @ 05,92  SAY "VLR. PAGTO."
      @ 05,112 SAY "OCORR."
      @ 05,122 SAY "NUM. LANC."
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
         @ LIN,58  SAY VLTI_TRN01 PICT "@R 999,999,999,999.99"
         @ LIN,80  SAY eDATA_PGTO
         @ LIN,92  SAY VPRI_TRN01 PICT "@R 999,999,999,999.99"
         @ LIN,112 SAY CDOC_TRN01 PICT "@R 99"
         @ LIN,122 SAY NSEQ_TRN01 PICT "@R 99999"
         @ LIN,134 SAY "|"
         LIN = LIN + 1
         eTOT_TIT = eTOT_TIT + VLTI_TRN01
         eTOT_CRE = eTOT_CRE + VPRI_TRN01
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
         @ LIN,58  SAY eTOT_TIT PICT "@R 999,999,999,999.99" 
         @ LIN,92  SAY eTOT_CRE PICT "@R 999,999,999,999.99"
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
