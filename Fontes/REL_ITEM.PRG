*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_ITEM.PRG
* Data        : 01/01/99
* Objetivo    : Relatário de Itens de Estoque
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_ITEM()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO ITENS DE ESTOQUE")
   PLQUA("01")
   PPERG ( "Deseja Relatorio em Ordem Alfabetica (S/N) ? ")
   IF sisRESP = "N"
      SELE 1
      USE ARQ_ITEM INDEX ARQ_ITEM,ITEM_DES
      GOTO TOP
   ELSE
      SELE 1
      USE ARQ_ITEM INDEX ITEM_DES,ARQ_ITEM
      GOTO TOP
   ENDIF
	eARQUIVO = SPACE(60)
	IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
		eARQUIVO = sisDRIVE_REL+":"
	ELSE
		eARQUIVO = "C:"
	ENDIF	
	
	IF sisPASTA_REL <> SPACE(30)
		eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"ITENS.TXT"+SPACE(10)  
	ELSE
		eARQUIVO = ALLTRIM(eARQUIVO) + "ITENS.TXT"+SPACE(10) 
	ENDIF        
   PLQUA("01")
   PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
   PPARA()
   DO PIMPR
   SET DEVICE TO PRINT
   NPAG = 1
   DO WHILE .NOT. EOF()
      @ 00,00  SAY sisLETRAC+" "
      @ 01,00  SAY sisTIT1
      @ 01,115 SAY "Pagina: "+STR(NPAG,8)
      @ 02,00  SAY sisTIT2
      @ 02,115 SAY "Data  : "+DTOC(DATE())
      @ 03,50  SAY "ITENS DE ESTOQUE"
      @ 03,50  SAY "ITENS DE ESTOQUE"
      @ 04,00  SAY REPLICATE("=",132)
      @ 05,00  SAY "Codigo"
      @ 05,10  SAY "Descricao"
      @ 05,55  SAY "UN"
      @ 05,60  SAY "Tipo"
      @ 05,75  SAY "Est.Minimo"
      @ 05,90  SAY "Ult.Entrada"
      @ 05,105 SAY "Ult.Saida"
      @ 06,00  SAY REPLICATE("=",132)
      LIN = 07
      DO WHILE LIN <= 58 .AND. .NOT. EOF()
         @ LIN,00  SAY CODIGO     PICT "@R 99999999"
         @ LIN,10  SAY ALLTRIM(DESCRICAO)
         @ LIN,55  SAY UNIDADE    PICT "XX"
         @ LIN,60  SAY TIPO       PICT "@R 999"
         @ LIN,75  SAY MINIMO     PICT "@R 999,999"
         @ LIN,90  SAY ULT_ENTR
         @ LIN,105 SAY ULT_SAID
	 SKIP
         DO PTECLA
         IF sisINTERR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN nil
         ENDIF	
	 LIN = LIN + 1
      ENDDO
      NPAG = NPAG + 1
   ENDDO
   @ 56,00  SAY REPLICATE("=",132)
   @ 57,90  SAY sisTIT3
   @ 00,00  SAY sisLETRAN+" "
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************

