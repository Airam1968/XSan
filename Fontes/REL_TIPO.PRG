*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_TIPO.PRG
* Data        : 01/01/99
* Objetivo    : Relatário de Tipo de Itens de Estoque
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_TIPO()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO TIPO DE ITENS DE ESTOQUE")
   SELE 1
   USE ARQ_TIPO INDEX ARQ_TIPO
   GOTO TOP
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
   PLQUA("01")
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   NPAG = 1
   DO WHILE .NOT. EOF()
      @ 01,00 SAY sisLETRAC+"Empresa : "+sisTIT1
      @ 01,120 SAY "Data : "+dtoc(DATE())
      @ 02,00 SAY "Sistema : "+sisTIT2
      @ 02,121 SAY "Relat: SCL001"
      @ 03,40 SAY sisLETRAE+"TABELA DE TIPO"+sisLETRAC
      @ 04,00 SAY REPLICATE("=",135)
      @ 05,00  SAY "|"
      @ 05,02  SAY "CODIGO"
      @ 05,15  SAY "DESCRICAO"
      @ 06,00 SAY REPLICATE("=",135)
      LIN = 7
      DO WHILE LIN <= 58 .AND. .NOT. EOF()
         @ LIN,00  SAY CODIGO     PICT "@R 99999999"
         @ LIN,15  SAY DESCRICAO
	 SKIP
         DO PTECLA
         IF sisINTERR
            SET PRINT TO
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF	
	 LIN = LIN + 1
      ENDDO
      NPAG = NPAG + 1
   ENDDO
   @ 56,00  SAY REPLICATE("=",132)
   @ 57,110 SAY sisTIT3
   @ 00,00  SAY sisLETRAN+" "
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************

