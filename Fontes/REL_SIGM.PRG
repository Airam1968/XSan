*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_SIGM.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE DADOS RECEBIDOS - SIGMA ITAU - POR CODIGO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_SIGM()
PPERG ( "Confirma (S/N) ?" )
IF sisRESP = "S"
   PTIT( "RELATORIO DADOS DE PAGAMENTO SIGMA" )
   PLQUA("01")
   eTIPO = SPACE(1)
   @ 23,02 SAY "Ordem Alfab‚tica ou Num‚rica (A/N)  : " GET eTIPO PICT "X" VALID eTIPO $("AN")
   READ
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
	eARQUIVO = SPACE(60)
	IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
		eARQUIVO = sisDRIVE_REL+":"
	ELSE
		eARQUIVO = "C:"
	ENDIF	
	
	IF sisPASTA_REL <> SPACE(30)
		eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
	ELSE
		eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
	ENDIF     
   DO PIMPR
   IF eTIPO = "A"
      SELECT 1
      SET EXCLUSIVE ON
      USE TRANSTXT 
      INDEX ON NOMS_TRN01 TO TRANSTXT
      GO TOP
      SET EXCLUSIVE OFF
   ELSE
      SELECT 1
      SET EXCLUSIVE ON
      USE TRANSTXT 
      INDEX ON SENU_TRN01 TO TRANSTXT
      GO TOP
      SET EXCLUSIVE OFF
   ENDIF
   eTOTAL = 0
   SET DEVICE TO PRINT
   DO WHILE .NOT. EOF()
      @ 01,00 SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00 SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SAN001"
      @ 03,40 SAY sisLETRAE+"RELATORIO DE PAGAMENTOS - SIGMA/ITAU"+sisLETRAC
      @ 04,00 SAY REPLICATE("=",135)
      @ 05,00  SAY "|"
      @ 05,02  SAY "NOSSO NUM."
      @ 05,14  SAY "NOME CLIENTE"
      @ 05,46  SAY "DATA VENC."
      @ 05,58  SAY "DATA PGTO."
      @ 05,70  SAY "VLR. PGTO."
      @ 05,90  SAY "LANC. ITAU"
      @ 05,134 SAY "|"
      @ 06,00 SAY REPLICATE("=",135)
      LIN = 7
      DO WHILE .NOT. EOF() .AND. LIN <= 57
         eDATA_VENC = ALLTRIM(STR(DTVE_TRN01))
         eDATA_PGTO = ALLTRIM(STR(DTCR_TRN01))
         eDATA_VENC = CTOD(SUBSTR(eDATA_VENC,1,2)+"/"+SUBSTR(eDATA_VENC,3,2)+"/"+SUBSTR(eDATA_VENC,5,2))
         eDATA_PGTO = CTOD(SUBSTR(eDATA_PGTO,1,2)+"/"+SUBSTR(eDATA_PGTO,3,2)+"/"+SUBSTR(eDATA_PGTO,5,2))
         @ LIN,00  SAY "|"
         @ LIN,02  SAY SENU_TRN01 PICT "@R 99999999"
         @ LIN,14  SAY ALLTRIM(NOMS_TRN01)
         @ LIN,46  SAY eDATA_VENC
         @ LIN,58  SAY eDATA_PGTO
         @ LIN,70  SAY VLTI_TRN01 PICT "@R 999,999,999,999.99"
         @ LIN,90  SAY NSEQ_TRN01 PICT "@R 99999"
         @ LIN,134 SAY "|"
         LIN = LIN + 1
         eTOTAL = eTOTAL + VLTI_TRN01
         SKIP
         DO PTECLA
         IF sisINTERR
            SET PRINT TO
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF
      ENDDO
      IF EOF()
         @ LIN,00 SAY REPLICATE("-",135)
         LIN = LIN + 1
         @ LIN,02  SAY "Total Geral : "
         @ LIN,70  SAY eTOTAL PICT "@R 999,999,999,999.99" 
      ENDIF
      @ 58,00 SAY REPLICATE("=",135)
      @ 59,104 SAY sisTIT3
      @ 01,00 SAY " "+sisLETRAN
   ENDDO
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************
