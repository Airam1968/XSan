*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RT2.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS DAS TABELAS DO CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RT2()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELATàRIOS DAS TABELAS DO CRECEBER")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 12,60 CLEAR TO 18,79
   @ 12,60 TO 18,79
   @ 13,61 PROMPT " 1     Bancos     " MESSAGE " Relatório de Tabela de Bancos do Sistema             "
   @ 14,61 PROMPT " 2   Contas Corr. " MESSAGE " Relatório de Tabela de Contas Correntes              "
   @ 15,61 PROMPT " 3    Clientes    " MESSAGE " Relatório de Tabela de Clientes do Contas a Receber  "
   @ 16,61 PROMPT " 4  Clas.Receitas " MESSAGE " Relatório de Tabela de Classificação de Receitas     "
   @ 17,61 PROMPT " Esc    Fim       " MESSAGE " Fim de Operação do Módulo                            "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_06")
		   REL_BANC()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - BANCOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_06")
		   REL_CCOR()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - C/C","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_06")
		   REL_CCTP()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_06")
		   REL_CLAR()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - CLAS.RECEITAS","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
