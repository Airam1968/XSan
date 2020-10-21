*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RCO.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS DE DADOS DE COBRANCA BANCARIA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_RCO()
dbcloseall()
SAVE SCREEN TO TELA10
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELATORIOS DE COBRANCA BANCµRIA")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 13,60 CLEAR TO 20,79
   @ 13,60 TO 20,79
   @ 14,61 PROMPT " 1      Geral     " MESSAGE " Relat¢rio de Arquivo de Retorno                    "
   @ 15,61 PROMPT " 2 Entrada Confirm" MESSAGE " Relat¢rio de Entradas Confirmada                   "
   @ 16,61 PROMPT " 3 Baixa Confimada" MESSAGE " Relat¢rio de Baixa Confirmada                      "
   @ 17,61 PROMPT " 4   Codigo Erro  " MESSAGE " Relat¢rio de Lançamentos com Codigos de Erro       "
   @ 18,61 PROMPT " 5 Tit. Protesto  " MESSAGE " Relat¢rio de T¡tulos Protestados                   "   
   @ 19,61 PROMPT " Esc     Fim      " MESSAGE " Fim de Opera‡Æo do M¢dulo                          "
   READ
   sisOP=1
   MENU TO sisOP
*   SAVE SCREEN TO TELA09
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_03")
	   REL_CNAG()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIO GERAL DE COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_03")
	   REL_CNAE()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIO ENTRADAS DE COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_03")
	   REL_CNAB()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIO BAIXAS DE COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_03")
	   REL_CNAR()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIO ERROS DE COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_03")
	   REL_CNAP()
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIO PROTESTO COBRANCA","ACESSO NEGADO")                        
	ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
	RESTORE SCREEN FROM TELA10
	CLOSE DATABASES
	RETURN nil
     ENDCASE
ENDDO
*************************  fim  *********************************
