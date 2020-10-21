*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_TCP.PRG
* Data        : 01/01/99
* Objetivo    : MENU TABELAS DO CANTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_TCP()
dbcloseall()
SAVE SCREEN TO TELA03
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU TABELAS DO CONTAS A PAGAR")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 10,50 CLEAR TO 16,69
   @ 10,50 TO 16,69
   @ 11,51 PROMPT " 1  Fornecedores  " MESSAGE " Tabelas de Fornecedores do Contas a Pagar         "
   @ 12,51 PROMPT " 2  Clas.Despesas " MESSAGE " Tabela de Classificaá∆o de Despesas               "
   @ 13,51 PROMPT " 3 Clas. Forneced." MESSAGE " Tabela de Classificacao de Fornecedores           "   
   @ 14,51 PROMPT " 4   Relat¢rios   " MESSAGE " Menu de Relat¢rios                                "
   @ 15,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                         "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_05")
		   TAB_FORN()
           dbcloseall()		   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - FORNECEDORES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_05")
		   TAB_CLAD()
           dbcloseall()		   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - CLAS.DESPESA","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CA_05")
		   TAB_CLAF()
           dbcloseall()		   
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - CLAS.FORNECEDOR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
		MENU_RT1()
           dbcloseall()		
     CASE LASTKEY() = 27
        dbcloseall()     
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA03
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
