*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_ESMI.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE ESTOQUE MINIMO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_ESMI()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO ESTOQUE MÖNIMO")
   PLQUA("01")
   PMENSAG("Aguarde, Elaborando relat¢rio de Ötens com estoque m¡nimo ...")
   SELE 3
   USE TIPO INDEX TIPO
   GOTO TOP
   SELE 2
   USE STATUS
   GOTO TOP
   eULT_ITEM = ULT_ITEM
   SELE 4
   USE ITENS INDEX ITENS ALIAS S4
   GOTO TOP
   SELE 1
   USE ESTOQUE INDEX ESTOQUE ALIAS S1
   SET RELATION TO CODIGO INTO S4
   GOTO TOP
   SELE 5
   USE ESTO_MIN INDEX ESTO_MIN
   ZAP
   SELE 1
   GOTO TOP
   DO WHILE .NOT. EOF()
      IF S4->MINIMO <= SALDO_FINA
	 SELE 5
	 APPEND BLANK
	 REPLACE CODIGO    WITH S1->CODIGO,;
		 DESCRICAO WITH S4->ITENS,;
                 MINIMO    WITH S4->MINIMO
	 REPLACE SALDO     WITH S1->SALDO_FINA
	 SELE 1
      ENDIF
      SKIP
   ENDDO
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
      @ 03,45  SAY "ESTOQUE MINIMO DE MATERIAIS"
      @ 03,45  SAY "ESTOQUE MINIMO DE MATERIAIS"
      @ 04,00  SAY REPLICATE("=",132)
      @ 05,00  SAY "Codigo"
      @ 05,10  SAY "Descricao"
      @ 05,60  SAY "SD.Minimo"
      @ 05,75  SAY "SD.Atual"
      @ 06,00  SAY REPLICATE("=",132)
      LIN = 07
      DO WHILE LIN <= 58 .AND. .NOT. EOF()
         @ LIN,00  SAY A->CODIGO     PICT "@R 99999999"
	 @ LIN,10  SAY ALLTRIM(A->DESCRICAO)
	 @ LIN,60  SAY A->MINIMO     PICT "@R 999,999"
	 @ LIN,75  SAY A->SALDO      PICT "@R 999,999"
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
   @ 57,110 SAY "McQuay Informatica"
   @ 00,00  SAY sisLETRAN+" "
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************

