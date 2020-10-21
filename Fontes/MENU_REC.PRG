*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_REC.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO RELATORIOS DE CAIXA E BANCOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_REC()
   dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELAT‡RIOS DE CAIXA E BANCOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 20,69
   @ 11,50 TO 20,69
   @ 12,51 PROMPT " 1 Cheques Compe. " MESSAGE " Relat¢rio de Cheques Compensados          "
   @ 13,51 PROMPT " 2 Cheques N Compe" MESSAGE " Relat¢rio de Cheques N∆o Compensados      "
   @ 14,51 PROMPT " 3 Deposito Compe." MESSAGE " Relat¢rio de Deposito Compensados         "
   @ 15,51 PROMPT " 4 DepositoN.Compe" MESSAGE " Relat¢rio de Deposito N∆o Compensados     "
   @ 16,51 PROMPT " 5 Extrato Movim. " MESSAGE " Relat¢rio de Extrato de Movimentaá∆o      "
   @ 17,51 PROMPT " 6 Resumo Contas  " MESSAGE " Relat¢rio de Resumo de Caixas             "
   @ 18,51 PROMPT " 7 Controle Caixa " MESSAGE " Relat¢rio de Controle de Caixa Di†rio     "
   @ 19,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_07")
		   REL_CHCO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CAIXA - CHEQUES COMP.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_07")
		   REL_CHNC()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CAIXA - CHEQUES NAO COMP.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_07")
		   REL_DECO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CAIXA - DEPOSITOS COMP.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_07")
		   REL_DENC()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CAIXA - DEPOSITOS NAO COMP.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_07")
		   REL_EXCA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CAIXA - EXTRATO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_RE_07")
		   REL_RECA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO RESUMO DE CAIXA","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_RE_07")
		   REL_CAIX()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CONTROLE DE CAIXA","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
