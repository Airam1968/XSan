*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CMP.PRG
* Data        : 01/01/99
* Objetivo    : MENU CONTROLE DE COMPRAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_CMP()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU CONTROLE DE COMPRAS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 16,60
   @ 09,40       TO 16,60
   @ 10,41 PROMPT " 1   Requisiá‰es   " MESSAGE " Menu de Requisis∆o de Material            "
   @ 11,41 PROMPT " 2    Cotaá‰es     " MESSAGE " Menu de Manutená∆o de Cotaá‰es            "
   @ 12,41 PROMPT " 3    Consultas    " MESSAGE " Menu de Consultas de Cotaá‰es             "
   @ 13,41 PROMPT " 4    Relatorios   " MESSAGE " Menu de Emiss∆o de Relat¢rios             "
   @ 14,41 PROMPT " 5    Tabelas      " MESSAGE " Menu de Manutená∆o de Tabelas             "
   @ 15,41 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do Sistema                "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
     CASE sisOP = 1
        MENU_PED()
     CASE sisOP = 2
        MENU_COT()
     CASE sisOP = 3
        MENU_PES()
     CASE sisOP = 4
        MENU_RCM()
     CASE sisOP = 5
        MENU_TAB()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA02
ENDDO
RETURN nil
*************************  fim  *********************************
