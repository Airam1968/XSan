*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CCO.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CONSULTA DE DADOS DE COBRANCA BANCARIA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_CCO()
dbcloseall()
SAVE SCREEN TO TELA09
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU CONSULTA DE COBRANCA BANCµRIA")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 13,60 CLEAR TO 21,79
   @ 13,60 TO 21,79
   @ 14,61 PROMPT " 1 Consulta Geral " MESSAGE " Consulta de Arquivo de Retorno                     "
   @ 15,61 PROMPT " 2 Entrada Confirm" MESSAGE " Consulta de Entradas Confirmada                    "
   @ 16,61 PROMPT " 3 Baixa Confimada" MESSAGE " Consulta de Baixa Confirmada                       "
   @ 17,61 PROMPT " 4   Codigo Erro  " MESSAGE " Consulta de Lanáamentos com Codigos de Erro        "
   @ 18,61 PROMPT " 5 Tit. Protesto  " MESSAGE " Consulta de Titulos em Protesto                    "
   @ 19,61 PROMPT " 6  N£mero Boleto " MESSAGE " Consulta de N£mero de Boletos                      "
   @ 20,61 PROMPT " Esc     Fim      " MESSAGE " Fim de Operaá∆o do M¢dulo                          "
   READ
   sisOP=1
   MENU TO sisOP
*   SAVE SCREEN TO TELA09
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CO_03")
	   CON_CNAG()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CONSULTA GERAL DE COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CO_03")
	   CON_CNAE()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CONSULTA ENTRADAS CONFIRMADAS","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CO_03")
	   CON_CNAB()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CONSULTA BAIXA DE PAGAMENTOS","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CO_03")
	   CON_CNAR()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CONSULTA DE ERROS DE COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CO_03")
	   CON_CNAP()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"CONSULTA DE PROTESTOS COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_CO_03")
           CON_BOLE()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE CODIGO DE BOLETOS","ACESSO NEGADO")                        
	ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
	RESTORE SCREEN FROM TELA09
	CLOSE DATABASES
	RETURN nil
     ENDCASE
ENDDO
*************************  fim  *********************************
