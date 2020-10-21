*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CNAE.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO COBRANCA BANCARIA - ARQUIVO RETORNO - PROTESTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_CNAP()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO COBRANCA BANCARIA - ARQUIVO RETORNO - PROTESTO")
   PLQUA("01")
   SET EXCLUSIVE ON
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
   DO WHILE .T.
      SET COLOR TO W/N,+W/BG
      @ 15,60 CLEAR TO 20,79
      @ 15,60 TO 20,79
      @ 16,61 PROMPT " 1  Liquida‡Æo    " MESSAGE " T¡tulos Liquidados em Cart¢rio                     "
      @ 17,61 PROMPT " 2 Baixa por Prot." MESSAGE " T¡tulo Efetivamente Protestado                     "
      @ 18,61 PROMPT " 3 T¡tulo Protest." MESSAGE " T¡tulo Encaminhado ao Cart¢rio                     "
      @ 19,61 PROMPT " 4 Susta‡Æo Prot. " MESSAGE " Instru‡Æo de Protesto Sustada                      "
      READ
      sisOP=1
      MENU TO sisOP
      DO CASE
         CASE sisOP = 1
            DO CASE
               CASE eCOD_BANCO = 1
                  eCOD_PROT = "07"
               CASE eCOD_BANCO = 8
                  eCOD_PROT = "15"
               CASE eCOD_BANCO = 33
                  eCOD_PROT = "  "
               CASE eCOD_BANCO = 104
                  eCOD_PROT = "22"
               CASE eCOD_BANCO = 215
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 230
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 237
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 275
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 291
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 341
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 409
                  eCOD_PROT = "89"
               CASE eCOD_BANCO = 641
                  eCOD_PROT = "15"
            ENDCASE
            EXIT
         CASE sisOP = 2
            DO CASE
               CASE eCOD_BANCO = 1
                  eCOD_PROT = "23"
               CASE eCOD_BANCO = 8
                  eCOD_PROT = "33"
               CASE eCOD_BANCO = 33
                  eCOD_PROT = "  "
               CASE eCOD_BANCO = 104
                  eCOD_PROT = "25"
               CASE eCOD_BANCO = 215
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 230
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 237
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 275
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 291
                  eCOD_PROT = "08"
               CASE eCOD_BANCO = 341
                  eCOD_PROT = "32"
               CASE eCOD_BANCO = 409
                  eCOD_PROT = "41"
               CASE eCOD_BANCO = 641
                  eCOD_PROT = "50"
            ENDCASE
            EXIT
         CASE sisOP = 3
            DO CASE
               CASE eCOD_BANCO = 1
                  eCOD_PROT = "23"
               CASE eCOD_BANCO = 8
                  eCOD_PROT = "33"
               CASE eCOD_BANCO = 33
                  eCOD_PROT = "  "
               CASE eCOD_BANCO = 104
                  eCOD_PROT = "26"
               CASE eCOD_BANCO = 215
                  eCOD_PROT = "15"
               CASE eCOD_BANCO = 230
                  eCOD_PROT = "15"
               CASE eCOD_BANCO = 237
                  eCOD_PROT = "15"
               CASE eCOD_BANCO = 275
                  eCOD_PROT = "15"
               CASE eCOD_BANCO = 291
                  eCOD_PROT = "15"
               CASE eCOD_BANCO = 341
                  eCOD_PROT = "32"
               CASE eCOD_BANCO = 409
                  eCOD_PROT = "41"
               CASE eCOD_BANCO = 641
                  eCOD_PROT = "10"
            ENDCASE
            EXIT
         CASE sisOP = 4
            DO CASE
               CASE eCOD_BANCO = 1
                  eCOD_PROT = "24"
               CASE eCOD_BANCO = 8
                  eCOD_PROT = "26"
               CASE eCOD_BANCO = 33
                  eCOD_PROT = "  "
               CASE eCOD_BANCO = 104
                  eCOD_PROT = "27"
               CASE eCOD_BANCO = 215
                  eCOD_PROT = "20"
               CASE eCOD_BANCO = 230
                  eCOD_PROT = "20"
               CASE eCOD_BANCO = 237
                  eCOD_PROT = "20"
               CASE eCOD_BANCO = 275
                  eCOD_PROT = "20"
               CASE eCOD_BANCO = 291
                  eCOD_PROT = "20"
               CASE eCOD_BANCO = 341
                  eCOD_PROT = "20"
               CASE eCOD_BANCO = 409
                  eCOD_PROT = "99"
               CASE eCOD_BANCO = 641
                  eCOD_PROT = "20"
            ENDCASE
            EXIT
      ENDCASE
   ENDDO
   DO CASE
      CASE eCOD_BANCO = 1
         SELE 1
         USE TRANS001
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 8
         SELE 1
         USE TRANS008
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 033
         SELE 1
         USE TRANS033
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = "  "
         GO TOP
      CASE eCOD_BANCO = 104
         SELE 1
         USE TRANS104
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 215
         SELE 1
         USE TRANS215
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 230
         SELE 1
         USE TRANS230
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 237
         SELE 1
         USE TRANS291
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 275
         SELE 1
         USE TRANS275
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 291
         SELE 1
         USE TRANS291
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 341
         SELE 1
         USE TRANS341
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 409
         SELE 1
         USE TRANS409
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = eCOD_PROT
         GO TOP
      CASE eCOD_BANCO = 424
         SELE 1
         USE TRANS424
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = "  "
         GO TOP
      CASE eCOD_BANCO = 641
         SELE 1
         USE TRANS641
         GOTO TOP
         SET FILTER TO CDOC_TRN01 = "  "
         GO TOP
      OTHERWISE
         SELE 1
         USE TRANSTXT
         GOTO TOP
   ENDCASE
   eTIPO = SPACE(1)
   @ 23,02 SAY "Ordem Alfab‚tica ou Num‚rica (A/N)  : " GET eTIPO PICT "X" VALID eTIPO $("AN")
   READ
   PLQUA("01")
   PMENSAG("Posicione o Formulario 80 col. e tecle [ENTER], para continuar...")
   PPARA()
   DO PIMPR
   eTOTAL = 0
   SET DEVICE TO PRINT
   DO WHILE .NOT. EOF()
      @ 01,00 SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,115 SAY "Data : "+dtoc(DATE())
      @ 02,00 SAY "Sistema : "+sisTIT2
      @ 02,115 SAY "Relat: SAN001"
      @ 03,30 SAY sisLETRAE+"RELATORIO DE COBRANCA BANCARIA - PROTESTOS - "+eNOME_ARQ+sisLETRAC
      @ 04,00 SAY REPLICATE("=",135)
      @ 05,00  SAY "|"
      @ 05,02  SAY "NOSSO NUM."
      @ 05,14  SAY "NOME CLIENTE"
      @ 05,46  SAY "DATA VENC."
      @ 05,60  SAY "VLR. TITULO"
      @ 05,90  SAY "OCORR."
      @ 05,100 SAY "NUM. LANC."
      @ 05,115 SAY "COD.BOLETO"
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
         @ LIN,100 SAY NSEQ_TRN01 PICT "@R 99999"
         DO CASE
            CASE eCOD_BANCO = 291
               @ LIN,120 SAY FIL2_TRN01 PICT "@R 99999999999999"
            CASE eCOD_BANCO = 641
               @ LIN,119 SAY NONU_TRN01 PICT "@R 999999999999999"
            OTHERWISE
               @ LIN,120 SAY NONU_TRN01 PICT "@R 99999999999999"
         ENDCASE
         @ LIN,134 SAY "|"
         LIN = LIN + 1
         eTOTAL = eTOTAL + VLTI_TRN01
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
         @ LIN,60  SAY eTOTAL PICT "@R 999,999,999,999.99" 
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
