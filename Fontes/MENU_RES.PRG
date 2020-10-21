*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RES.PRG
* Data        : 01/01/99
* Objetivo    : Menu do Modulo de Relatários
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RES()
DO WHILE .T.
   dbcloseall()
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELAT‡RIOS DE ESTOQUE")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 10,50 CLEAR TO 15,68
   @ 10,50 TO 15,68
   @ 11,51 PROMPT " 1    Estoque    " MESSAGE " Relat¢rio de Estoque                                     "
   @ 12,51 PROMPT " 2 Estoque M°nimo" MESSAGE " Relat¢rio de Estoque M°nimo                              "
   @ 13,51 PROMPT " 3     Itens     " MESSAGE " Relat¢rio de Itens de Estoque                            "
   @ 14,51 PROMPT " Esc    Fim      " MESSAGE " Fim de Utilizaá∆o do M¢dulo                              "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA02
   DO CASE
     CASE sisOP = 1
        DO REL_ESTO
     CASE sisOP = 2
        DO REL_ESMI
     CASE sisOP = 3
        DO REL_ITEM
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA02
ENDDO
*************************  fim  *********************************
