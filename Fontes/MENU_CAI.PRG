*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CAI.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CAIXA E BANCOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_CAI()
DO WHILE .T.
   dbcloseall()
   PTIT("MENU CAIXA E BANCOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 13,60
   @ 09,40 TO 13,60
   @ 10,41 PROMPT " 1    Consultas    " MESSAGE " Menu de Consultas de Caixa/Bancos            "
   @ 11,41 PROMPT " 2    Relat¢rios   " MESSAGE " Menu de Relatorios de Caixa/Bancos           "
   @ 12,41 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA03
   DO CASE
     CASE sisOP = 1
        MENU_COC()
     CASE sisOP = 2
        MENU_REC()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
	RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA03
ENDDO
RETURN nil
*************************  fim  *********************************
