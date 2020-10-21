*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RTC.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS DAS TABELAS DO CONTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RTC()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELAT‡RIOS DAS TABELAS DO CPAGAR/CRECEBER")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 12,60 CLEAR TO 20,79
   @ 12,60 TO 20,79
   @ 13,61 PROMPT " 1     Bancos     " MESSAGE " Relat¢rio de Tabela de Bancos do Sistema             "
   @ 14,61 PROMPT " 2   Contas Corr. " MESSAGE " Relat¢rio de Tabela de Contas Correntes              "
   @ 15,61 PROMPT " 3    Clientes    " MESSAGE " Relat¢rio de Tabela de Clientes do Contas a Receber  "
   @ 16,61 PROMPT " 4  Fornecedores  " MESSAGE " Relat¢rio de Tabela de Fornecedores do Contas a Pagar"
   @ 17,61 PROMPT " 5  Clas.Despesas " MESSAGE " Relat¢rio de Tabela de Classificaá∆o de Despesas     "
   @ 18,61 PROMPT " 6  Clas.Receitas " MESSAGE " Relat¢rio de Tabela de Classificaá∆o de Receitas     "
   @ 19,61 PROMPT " Esc    Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                            "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("RELATORIOS")
		   REL_BANC()
	   dbcloseall()	   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - BANCOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("RELATORIOS")
		   REL_CCOR()
           dbcloseall() 
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - C/C","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("RELATORIOS")
		   REL_CCTP()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("RELATORIOS")
		   REL_FORN()
	   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - FORNECEDORES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("RELATORIOS")
		   REL_CLAD()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - CLAS.DESPESAS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("RELATORIOS")
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
