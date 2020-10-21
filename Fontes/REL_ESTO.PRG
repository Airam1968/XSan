*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_ESTO.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE ESTOQUE DE MATERIAIS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_ESTO()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "RELATORIO ESTOQUE DE MATERIAIS")
   SELE 2
   USE ITENS INDEX ITENS
   SELE 1
   USE ESTOQUE INDEX ESTOQUE
   SET RELATION TO CODIGO INTO B
   GOTO TOP
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
      @ 03,50  SAY "ESTOQUE DE MATERIAIS"
      @ 03,50  SAY "ESTOQUE DE MATERIAIS"
      @ 04,00  SAY REPLICATE("=",132)
      @ 05,00  SAY "Codigo"
      @ 05,10  SAY "Descricao"
      @ 05,50  SAY "SD.Inicial"
      @ 05,65  SAY "Entradas"
      @ 05,80  SAY "Saidas"
      @ 05,95  SAY "SD. Final"
      @ 05,110 SAY "Ult.Mov."
      @ 06,00  SAY REPLICATE("=",132)
      LIN = 07
      DO WHILE LIN <= 58 .AND. .NOT. EOF()
         @ LIN,00  SAY A->CODIGO     PICT "@R 99999999"
	 @ LIN,10  SAY ALLTRIM(B->DESCRICAO)
	 @ LIN,50  SAY A->SALDO_INIC PICT "@R 999,999"
	 @ LIN,65  SAY A->ENTRADA    PICT "@R 999,999"
	 @ LIN,80  SAY A->SAIDA      PICT "@R 999,999"
	 @ LIN,95  SAY A->SALDO_FINA PICT "@R 999,999"
	 @ LIN,110 SAY A->MOVIMENTO
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

