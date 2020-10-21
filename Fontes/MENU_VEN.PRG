*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_VEN.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE VENDAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_VEN()
DO WHILE .T.
   dbcloseall()
   SET COLOR TO W/N,+W/R
   PTIT( "MENU VENDAS"
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 15,60
   @ 09,40 TO 15,60
   @ 10,41 PROMPT " 1    Aprova‡Æo    " MESSAGE " Aprova‡Æo de Pedido de Venda                 "
   @ 11,41 PROMPT " 2    Consultas    " MESSAGE " Menu Consultas de Posi‡Æo de Vendas          "
   @ 12,41 PROMPT " 3   Relat¢rios    " MESSAGE " Menu EmissÆo de Relat¢rios                   "
   @ 13,41 PROMPT " 4    Tabelas      " MESSAGE " Menu Tabelas de Vendas                       "
   @ 14,41 PROMPT " Esc    Fim        " MESSAGE " Fim de Opera‡Æo do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
     CASE sisOP = 1
        DO PDESENV
*       DO APR_PEDV
     CASE sisOP = 2
        DO PDESENV
*       DO MENU_CVD
     CASE sisOP = 3
        DO PDESENV
*       DO MENU_RVD
     CASE sisOP = 4
        DO PDESENV
*       DO MENU_TVD
     CASE LASTKEY() = 27
       dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA01
ENDDO
*************************  fim  *********************************
