*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_PES.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CONSULTA DE COTACOES E COMPRAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_PES()
SAVE SCREEN TO TELA03
DO WHILE .T.
   PTIT( "MENU CONSULTA DE COTACOES E COMPRAS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 14,70
   @ 11,50 TO 14,70
   @ 12,51 PROMPT " 1 Situaá∆o Cotaá∆o" MESSAGE " Consulta a Situaá∆o da Cotaá∆o               "
   @ 13,51 PROMPT " Esc   Fim         " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA03
   DO CASE
     CASE sisOP = 1
        CON_SCOT()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA03
ENDDO
*************************  fim  *********************************
