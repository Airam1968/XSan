*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CTR.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_CTR()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU CONTAS A RECEBER")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,39 CLEAR TO 19,60
   @ 09,39 TO 19,60
   @ 10,41 PROMPT " 1    Cadastro    " MESSAGE " Cadastro de Contas a Receber            "
   @ 11,41 PROMPT " 2    Alteraá∆o   " MESSAGE " Alteraá∆o de Contas a Receber           "
   @ 12,41 PROMPT " 3    Exclus∆o    " MESSAGE " Exclus∆o de Contas a Receber            "
   @ 13,41 PROMPT " 4    Consultas   " MESSAGE " Consultas de Contas a Receber           "
   @ 14,41 PROMPT " 5    Relat¢rio   " MESSAGE " Emiss∆o do Relat¢rio de CReceber        "
   @ 15,41 PROMPT " 6   Autom†tico   " MESSAGE " Recepá∆o Autom†tica de Lanáamentos      "
   @ 16,41 PROMPT " 7    Tabelas     " MESSAGE " Menu Manutená∆o de Tabelas              "
   @ 17,41 PROMPT " 8   Importaá∆o   " MESSAGE " Importaá∆o de Lanáamentos               "    
   @ 18,41 PROMPT " Esc     Fim      " MESSAGE " Fim de Operaá∆o do M¢dulo               "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_06")
	   CAD_CTRE()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CADASTRO DE CONTAS A RECEBER","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_AL_06")
	   ALT_CTRE()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"ALTERACAO DE CONTAS A RECEBER","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_EX_06")
	   EXC_CTRE()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"EXCLUSAO DE CONTAS A RECEBER","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 4
	MENU_CRE()
     CASE sisOP = 5
	MENU_RCR()
     CASE sisOP = 6
        IF F_ACESSO("DI_CA_06")
	   AUT_CTRE()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"LANCAMENTO AUTOM. CRECEBER","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 7
        MENU_TCR()
     CASE sisOP = 8
        IMP_CTRC()          
     CASE LASTKEY() = 27
        dbcloseall()
	CLOSE DATABASES
	RESTORE SCREEN FROM TELA04
	RETURN nil
  ENDCASE
ENDDO
RETURN nil
*************************  fim  *********************************
