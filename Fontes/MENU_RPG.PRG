*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RPG.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO RELATORIOS DE CONTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RPG()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELAT‡RIOS DO CONTAS A PAGAR")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,50 CLEAR TO 21,69
   @ 09,50 TO 21,69
   @ 10,51 PROMPT " 1     Geral      " MESSAGE " Relat¢rio de T°tulos por Per°odo          "
   @ 11,51 PROMPT " 2 Tit. em Aberto " MESSAGE " Relat¢rio de T°tulos em Aberto            "
   @ 12,51 PROMPT " 3 Tit. Liquidados" MESSAGE " Relat¢rio de T°tulos Liquidados           "
   @ 13,51 PROMPT " 4   Por Classe   " MESSAGE " Relat¢rio de T°tulos de Classe de Despesa "
   @ 14,51 PROMPT " 5 Por Fornecedor " MESSAGE " Relat¢rio de T°tulos por Fornecedor       "
   @ 15,51 PROMPT " 6 Produto/Classe " MESSAGE " Relat¢rio por Empreendimento e Classe Desp"
   @ 16,51 PROMPT " 7 Produto/Fornec." MESSAGE " Relat¢rio por Empreendimento e Fornecedor "
   @ 17,51 PROMPT " 8     Cheques    " MESSAGE " Cheques em Formul†rio Cont°nuo Individual "
   @ 18,51 PROMPT " 9 Cheques Agrup. " MESSAGE " Cheques em Formul†rio Cont°nuo Agrupados  "
   @ 19,51 PROMPT "10 Emiss∆o A.P.s  " MESSAGE " Emiss∆o de Autorizaá∆o de Pagamento       "
   @ 20,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_05")
		   REL_CTPG()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - GERAL","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_05")
		   REL_CTPA()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - T.ABERTO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_05")
		   REL_CTPB()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - BAIXAS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_05")
		   REL_CTPC()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - CLASSE","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_05")
		   REL_CTPF()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - FORNECEDOR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6     
        IF F_ACESSO("DI_RE_05")
		   REL_CTPE()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR -    PRODUTO    ","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_RE_05")
		   REL_CTP2()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR -    PRODUTO    ","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 8
        IF F_ACESSO("DI_RE_05")
           * EMISSAO DE CHEQUE DE VEC VENENIANI
           IF sisCGC_CPF = "38953519000167"
			  REL_CHEV()
           ELSE
			  REL_CHEQ()
           ENDIF
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - CHEQUES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 9
        IF F_ACESSO("DI_RE_05")
           * EMISSAO DE CHEQUE DE VEC VENENIANI
           IF sisCGC_CPF = "38953519000167"
			  REL_CHAV()
           ELSE
			  REL_CHEA()
           ENDIF
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - CHEQUES AGRUP.","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 10
        IF F_ACESSO("DI_RE_05")
           REL_APAG()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CPAGAR - AUT.PAGAMENTO","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************

