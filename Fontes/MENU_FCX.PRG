*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_FCX.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO FLUXO DE CAIXA  
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_FCX()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU FLUXO DE CAIXA")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 07,39 CLEAR TO 19,64
   @ 07,39 TO 19,64                   
   @ 08,41 PROMPT " 1 Previsto Diario 1  " MESSAGE " Relat¢rio de Fluxo de Caixa diario Vertical       "
   @ 09,41 PROMPT " 2 Previsto Diario 2  " MESSAGE " Relat¢rio de Fluxo de Caixa diario Horizontal     "
   @ 10,41 PROMPT " 3 Tot.Previsto Mensal" MESSAGE " Relat¢rio de Fluxo de Caixa Total Previsto Mensal "
   @ 11,41 PROMPT " 4 Tot.Realizad.Mensal" MESSAGE " Relat¢rio de Fluxo de Caixa Total Realizado Mensal"
   @ 12,41 PROMPT " 5 Previsto Mensal    " MESSAGE " Relat¢rio de Fluxo de Caixa Previsto Mensal       "
   @ 13,41 PROMPT " 6 Realizad.Mensal    " MESSAGE " Relat¢rio de Fluxo de Caixa Realizado Mensal      "
   @ 14,41 PROMPT " 7 Previsto Trimestral" MESSAGE " Relat¢rio de Fluxo de Caixa Previsto Trimestral   "
   @ 15,41 PROMPT " 8 Realizad.Trimestral" MESSAGE " Relat¢rio de Fluxo de Caixa Prev.+ Real Trimestre "
   @ 16,41 PROMPT " 9  Modelo Balancete  " MESSAGE " Relat¢rio de Fluxo de Caixa Modelo Balancete      "
   @ 17,41 PROMPT "10   Modelo Rateio    " MESSAGE " Relat¢rio de Fluxo de Caixa Modelo Rateio         "
   @ 18,41 PROMPT " Esc     Fim          " MESSAGE " Fim de Operaá∆o do M¢dulo                         "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_08")
            REL_FLD1()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - PREVISTO","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_08")
            REL_FLD2()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - PREVISTO","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_08")
            REL_FLP2()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA TOTAL - PREVISTO","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_08")
            REL_FLR2()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA TOTAL - REALIZADO TRIM.","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_08")
           REL_FLPM()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - PREVISTO","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_RE_08")
            REL_FLRM()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - REALIZADO TRIM.","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_RE_08")
		   REL_FLPT()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - PREVISTO TRIM.","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 8
        IF F_ACESSO("DI_RE_08")
           REL_FLRT()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
	   F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - REALIZADO","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 9
        IF F_ACESSO("DI_RE_08")
           REL_FLBL()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - BALANCETE","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 10
        IF F_ACESSO("DI_RE_08")
           REL_FLRA()
	ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS FLUXO CAIXA - RATEIO","ACESSO NEGADO")
	ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
	CLOSE DATABASES
	RESTORE SCREEN FROM TELA04
	RETURN nil
  ENDCASE
ENDDO
RETURN nil
*************************  fim  *********************************
