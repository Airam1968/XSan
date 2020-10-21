*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CON.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS CONSOLIDADOS DE CPAGAR\CRECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_CON()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELAT‡RIOS CONSOLIDADOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 08,39 CLEAR TO 13,60
   @ 08,39 TO 13,60
   @ 09,41 PROMPT " 1  Fluxo  Di†rio " MESSAGE " Relat¢rio de Fluxo de Caixa Di†rio        "
   @ 10,41 PROMPT " 2  Fluxo Semanal " MESSAGE " Relat¢rio de Fluxo de Caixa Semanal       "
   @ 11,41 PROMPT " 3  Fluxo  Mensal " MESSAGE " Alteraá∆o de Contas a Pagar               "
   @ 12,41 PROMPT " Esc     Fim      " MESSAGE " Fim de Operaá∆o do M¢dulo                 "
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




