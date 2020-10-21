*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TESTE.PRG
* Data        : 01/01/99
* Objetivo    : ---
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TESTE()
eNOME_EMPRE = SPACE(50)
SELE 6
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
DO WHILE .T.
   eCLASSE_PRO = 0
   PLQUA("01")
   @ 23,00 SAY "   Produto     ou (0) para Todos : " GET eCLASSE_PRO PICT "@R 9999999"
   READ
   SELE 6
   SEEK eCLASSE_PRO
   IF .NOT. FOUND()
      IF eCLASSE_PRO <> 0
         PLQUA("01")
         PMSGR( "  Produto   Inexistente   ",3
         SELE 6
         LOOP
      ELSE
         @ 23,48 SAY " - Escritorio"
         SELE 6
         EXIT
      ENDIF
   ELSE
      @ 23,48 SAY " - "+ALLTRIM(DESCRICAO)
      eNOME_EMPRE = DESCRICAO
      SELE 6
      EXIT
   ENDIF
ENDDO
RETURN NIL
