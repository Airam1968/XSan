*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_TES.PRG
* Data        : 01/01/99
* Objetivo    : Menu Modulo de Tabelas
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_TES()
DO WHILE .T.
   dbcloseall()
   SET COLOR TO W/N,+W/R
   PTIT( "MENU TABELAS DE ESTOQUES"
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 10,50 CLEAR TO 15,68
   @ 10,50 TO 15,68
   @ 11,51 PROMPT " 1     Tipo      " MESSAGE " Manutenção do Cadastro de Tipo de Ítens        "
   @ 12,51 PROMPT " 2     Ítens     " MESSAGE " Manutenção do Cadastro de Ítens                "
   @ 13,51 PROMPT " 3    Unidade    " MESSAGE " Manutenção do Cadastro de Unidade de Medida    "
   @ 14,51 PROMPT " Esc    Fim      " MESSAGE " Fim de Operação do Módulo                      "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA02
   DO CASE
      CASE sisOP = 1
         DO TAB_TIPO
      CASE sisOP = 2
         DO TAB_ITEM
      CASE sisOP = 3
         DO TAB_MEDI
      CASE LASTKEY() = 27
         dbcloseall()
         CLOSE DATABASES
         RETURN nil
   ENDCASE
   RESTORE SCREEN FROM TELA02
ENDDO
*************************  fim *********************************
