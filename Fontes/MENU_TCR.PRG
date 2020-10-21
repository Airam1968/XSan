*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_TCR.PRG
* Data        : 01/01/99
* Objetivo    : MENU TABELAS DO CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_TCR()
dbcloseall()
SAVE SCREEN TO TELA03
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU TABELAS DO CRECEBER")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 10,50 CLEAR TO 17,69
   @ 10,50 TO 17,69
   @ 11,51 PROMPT " 1     Bancos     " MESSAGE " Tabelas de Bancos do Sistema                      "
   @ 12,51 PROMPT " 2   Contas Corr. " MESSAGE " Tabelas de Contas Correntes                       "
   @ 13,51 PROMPT " 3    Clientes    " MESSAGE " Tabelas de Clientes do Contas a Receber           "
   @ 14,51 PROMPT " 4  Clas.Receitas " MESSAGE " Tabela de Classificaçõesde Receitas               "
   @ 15,51 PROMPT " 5   Relatórios   " MESSAGE " Menu de Relatórios                                "   
   @ 16,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Operação do Módulo                         "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_06")
		   TAB_BANC()
           dbcloseall()		   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - BANCOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_06")
		   TAB_CCOR()
           dbcloseall()		   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - C/C","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CA_06")
		   TAB_CLIE()
           dbcloseall()		   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CA_06")
		   TAB_CLAR()
           dbcloseall()		   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - CLAS.RECEITA","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
		MENU_RT2()
     CASE LASTKEY() = 27
        dbcloseall()     
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA03
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
