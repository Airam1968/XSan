*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_SIG.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CAPTURA DE DADOS DO SIGMA - ITAU
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_SIG()
dbcloseall()
SAVE SCREEN TO TELA03
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU COBRANCA ITAU-SIGMA"
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 18,70
   @ 11,50 TO 18,70
   @ 12,51 PROMPT " 1   Configuraá∆o  " MESSAGE " Configuraá∆o para Captura dos Dados            "
   @ 13,51 PROMPT " 2     Captura     " MESSAGE " Captura dos dados SIGMA-ITAá                   "
   @ 14,51 PROMPT " 3    Consulta     " MESSAGE " Consulta dos Dados SIGMA-ITAá                  "
   @ 15,51 PROMPT " 4   Relatorio     " MESSAGE " Emissao de Relatorio de Consistencia           "
   @ 16,51 PROMPT " 5 Lanáamento Aut. " MESSAGE " Lanáamento Automático de Pagamentos Itau       "   
   @ 17,51 PROMPT " Esc    Fim        " MESSAGE " Fim de Operaá∆o do Mádulo                      "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
	IF F_ACESSO("ALTERACAO")
	   DO CONF_SIG
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	ENDIF
     CASE sisOP = 2
	IF F_ACESSO("ALTERACAO")
	   DO CAPT_SIG
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	ENDIF
     CASE sisOP = 3
	IF F_ACESSO("CONSULTAS")
	   DO CON_SIGM
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	ENDIF
     CASE sisOP = 4
	IF F_ACESSO("RELATORIOS")
	   DO REL_SIGM
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	ENDIF
     CASE sisOP = 5
	IF F_ACESSO("ALTERACAO")
	   DO PDESENV
*          DO REL_SIGM
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	ENDIF
     CASE LASTKEY() = 27
        dbcloseall()     
	RESTORE SCREEN FROM TELA03
	CLOSE DATABASES
	RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************

