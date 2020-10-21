*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_COB.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE GERENCIA DE DADOS DE COBRANCA BANCARIA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_COB()
dbcloseall()
SAVE SCREEN TO TELA07
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU GERENCIA DE COBRANÄA BANCµRIA")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 19,70
   @ 11,50 TO 19,70
   @ 12,51 PROMPT " 1   Configuraá∆o  " MESSAGE " Configuraá∆o para Captura dos Dados                "
   @ 13,51 PROMPT " 2 Arquivo Remessa " MESSAGE " Geraá∆o do Arquivo Remessa de Cobranáa             "
   @ 14,51 PROMPT " 3 Arquivo Retorno " MESSAGE " Captura dos Dados do Arquivo Retorno Cobranáa      "
   @ 15,51 PROMPT " 4    Consultas    " MESSAGE " Consulta dos Dados do Arquivo Retorno              "
   @ 16,51 PROMPT " 5   Relatorios    " MESSAGE " Menu de Relatorios de Consistància Retorno         "
   @ 17,51 PROMPT " 6 Lanáamento Aut. " MESSAGE " Baixa Autom†tica Arquivo Retorno nos Financiamentos"
   @ 18,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                          "
   READ
   sisOP=1
   MENU TO sisOP
*   SAVE SCREEN TO TELA07
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_AL_03")
	   CONF_COB()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CONFIGURACAO DA COBRANCA ELETRONICA","ACESSO NEGADO")                
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_03")
	   REME_COB()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"GERAÄ«O DO ARQUIVO DE REMESSA","ACESSO NEGADO")                
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CA_03")
	   CAPT_COB()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RECEPCAO DO ARQUIVO DE RETORNO","ACESSO NEGADO")                
	ENDIF
     CASE sisOP = 4
	MENU_CCO()
     CASE sisOP = 5
	MENU_RCO()
     CASE sisOP = 6
        IF F_ACESSO("DI_CA_03")
	   BAI_AUTO()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"BAIXA AUTOMATICA DE PAGAMENTOS","ACESSO NEGADO")                        
	ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
	RESTORE SCREEN FROM TELA07
	CLOSE DATABASES
	RETURN nil
     ENDCASE
ENDDO
*************************  fim  *********************************
