*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CES.PRG
* Data        : 01/01/99
* Objetivo    : Menu do Modulo de Consultas de Estoque
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
************************* inicio ************************************
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   DO PTIT WITH 'MENU CONSULTAS DE ESTOQUE'
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,50 CLEAR TO 13,68
   @ 09,50 TO 13,68
   @ 10,51 PROMPT ' 1    Estoque    ' MESSAGE ' Consulta de Dados de Estoque Geral                       '
   @ 11,51 PROMPT ' 2 Estoque M¡nimo' MESSAGE ' Consulta de Dados de Ötens com Estoque M¡nimo            '
   @ 12,51 PROMPT ' Esc     Fim     ' MESSAGE ' Fim de Utiliza‡ƒo do M¢dulo                              '
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA02
   DO CASE
     CASE sisOP = 1
        DO CON_ESTO
     CASE sisOP = 2
        DO CON_ESMI
     CASE LASTKEY() = 27
        CLOSE DATABASES
        RETURN
     ENDCASE
     RESTORE SCREEN FROM TELA02
ENDDO
*************************  fim  *********************************
