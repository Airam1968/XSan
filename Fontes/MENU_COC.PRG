*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_COC.PRG
* Data        : 01/01/99
* Objetivo    : MENU CONSULTA DE CAIXA E BANCOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_COC()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU CONSULTA DE CAIXA E BANCOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 19,69
   @ 11,50 TO 19,69
   @ 12,51 PROMPT " 1  Movimenta‡äes " MESSAGE " Consulta de Resumo de Saldos Di rios      "
   @ 13,51 PROMPT " 2 Cheques Compe. " MESSAGE " Consulta de Cheques Compensados           "
   @ 14,51 PROMPT " 3 Cheques N Compe" MESSAGE " Consulta de Cheques NÆo Compensados       "
   @ 15,51 PROMPT " 4 Deposito Compe." MESSAGE " Consulta de Deposito Compensados          "
   @ 16,51 PROMPT " 5 Deposito N Comp" MESSAGE " Consulta de Deposito NÆo Compensados      "
   @ 17,51 PROMPT " 6 Resumo Contas  " MESSAGE " Consulta de Resumo de Caixas              "
   @ 18,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Opera‡Æo do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CO_07")
           CON_SALD()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CAIXA - SALDOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CO_07")
           CON_CHCO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CAIXA - CHEQUES COMPE.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CO_07")
           CON_CHNC()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CAIXA - CHEQUES NAO COMPE.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CO_07")
           CON_DECO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CAIXA - DEPOSITO COMPE.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CO_07")
           CON_DENC()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CAIXA - DEPOSITO NAO COMPE.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_CO_07")
           CON_RECA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CAIXA - RESUMO DE CONTAS","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA04
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
