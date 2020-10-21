*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RRE.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RRE()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELATàRIOS CONTAS A RECEBER"
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 08,49 CLEAR TO 13,70
   @ 08,49 TO 13,70
   @ 09,51 PROMPT " 1     Diário     " MESSAGE " Relatório de Recebimentos Diários         "
   @ 10,51 PROMPT " 2    Semanal     " MESSAGE " Relatório de Recebimentos Semanal         "
   @ 11,51 PROMPT " 3     Mensal     " MESSAGE " Relatório de Recebimentos Mensais         "
   @ 12,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Operação do Módulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
     CASE sisOP = 2
     CASE sisOP = 3
     CASE LASTKEY() = 27
       dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
