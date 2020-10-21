*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_EST.PRG
* Data        : 01/01/99
* Objetivo    : MENU PRINCIPAL
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_EST()
DO WHILE .T.
   dbcloseall()
   SET COLOR TO W/N,+W/R
   PTIT( "MENU ESTOQUES")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 16,60
   @ 09,40 TO 16,60
   @ 10,41 PROMPT " 1  Movimentaá‰es  " MESSAGE " Movimentaá‰es de Estoque                     "
   @ 11,41 PROMPT " 2    Consultas    " MESSAGE " Consultas Gerais do Sistema                  "
   @ 12,41 PROMPT " 3   Relat¢rios    " MESSAGE " Emiss∆o de Relat¢rios                        "
   @ 13,41 PROMPT " 4     Tabelas     " MESSAGE " Menu de Manutená∆o das Tabelas do Sistema    "
   @ 14,41 PROMPT " 5   Fechamento    " MESSAGE " Fechamento Mensal de Estoque                 "
   @ 15,41 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
     CASE sisOP = 1
        DO MOVIMENT
     CASE sisOP = 2
        DO MENU_CES
     CASE sisOP = 3
        DO MENU_RES
     CASE sisOP = 4
        DO MENU_TES
     CASE sisOP = 5
        DO FECH_EST
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA01
ENDDO
*************************  fim  *********************************




