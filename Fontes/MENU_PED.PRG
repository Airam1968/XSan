*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_PED.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE PEDIDO DE MATERIAL
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_PED()
dbcloseall()
SAVE SCREEN TO TELA03
DO WHILE .T.
   PTIT( "MENU PEDIDO DE MATERIAL")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 18,70
   @ 11,50 TO 18,70
   @ 12,51 PROMPT " 1    Inclus∆o     " MESSAGE " Inclus∆o de Requisiá∆o no Sistema           "
   @ 13,51 PROMPT " 2    Alteraá∆o    " MESSAGE " Alteraá∆o de Requisiá∆o no Sistema          "
   @ 14,51 PROMPT " 3    Exclus∆o     " MESSAGE " Exclus∆o de Requisiá∆o no Sistema           "
   @ 15,51 PROMPT " 4    Consulta     " MESSAGE " Consulta de Requisiá∆o no Sistema           "   
   @ 16,51 PROMPT " 5    Relatorio    " MESSAGE " Emissao de Relat¢rio de Requisiá∆o Material "
   @ 17,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                   "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA03
   DO CASE
     CASE sisOP = 1
        INC_PEDC()
     CASE sisOP = 2
        ALT_PEDC()
     CASE sisOP = 3
        EXC_PEDC()
     CASE sisOP = 4
        CON_PEDC()
     CASE sisOP = 5
        REL_RECO()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA03
ENDDO
*************************  fim  *********************************
