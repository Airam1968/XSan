*********************************************************************
* Sistema     : Gestã®´o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_COT.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE COTACOES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_COT()
dbcloseall()
SAVE SCREEN TO TELA03
DO WHILE .T.
   PTIT( "MENU COTACOES")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 21,70
   @ 11,50 TO 21,70
   @ 12,51 PROMPT " 1    Inclus∆o     " MESSAGE " Inclus∆o de Cotaá‰es no Sistema              "
   @ 13,51 PROMPT " 2    Alteraá∆o    " MESSAGE " Alteraá∆o de Cotaá‰es no Sistema             "
   @ 14,51 PROMPT " 3    Exclus∆o     " MESSAGE " Exclus∆o de Cotaá‰es no Sistema              "
   @ 15,51 PROMPT " 4    Consulta     " MESSAGE " Consulta de Cotaá‰es no Sistema              "   
   @ 16,51 PROMPT " 5   Lanáamentos   " MESSAGE " Lanáamento de Valores de Cotaá∆o             "   
   @ 17,51 PROMPT " 6   Fechamentos   " MESSAGE " Fechamento de Cotaá∆o                        "   
   @ 18,51 PROMPT " 7 Desmembramento  " MESSAGE " Desmembramento da Cotaá∆o                    "   
   @ 19,51 PROMPT " 8   Vizualizacao  " MESSAGE " Visualizacao do Arquivo de Cotacoes          "   
   @ 20,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA03
   DO CASE
     CASE sisOP = 1
        DO INC_COTA
     CASE sisOP = 2
        DO ALT_COTA
     CASE sisOP = 3
        DO EXC_COTA
     CASE sisOP = 4
        DO CON_COTA
     CASE sisOP = 5
        DO LAN_COTA
     CASE sisOP = 6
        DO LAN_FECH
     CASE sisOP = 7
        DO DES_COTA
     CASE sisOP = 8
        DO VIS_COTA
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA03
ENDDO
*************************  fim  *********************************
