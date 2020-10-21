*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_IME.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO IMPORTACAO EXPORTACAO DE DADOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_IME
SAVE SCREEN TO TELA03
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU IMPORTAÄ«O/EXPORTAÄ«O DE DADOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 17,69
   @ 11,50 TO 17,69
   @ 12,51 PROMPT " 1 Contab.-ProSoft" MESSAGE " Exportaá∆o para Contabilidade Prosoft     "
   @ 13,51 PROMPT " 2 Caixa Economica" MESSAGE " Exportaá∆o para CEF                       "
   @ 14,51 PROMPT " 3 Exportaá∆o PIS " MESSAGE " Exportaá∆o para PIS/COFINS                "
   @ 15,51 PROMPT " 4  Registros PIS " MESSAGE " Manutená∆o Registros para PIS/COFINS      "
   @ 16,51 PROMPT " Esc     Fim      " MESSAGE " Fim de Operaá∆o do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA04
           EXP_PROS()
           RESTORE SCREEN FROM TELA04
	ELSE
	   PLQUA("01")
	   PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO EXPORT. PROSOFT","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA04
           EXP_CEF1()
           RESTORE SCREEN FROM TELA04
	ELSE
	   PLQUA("01")
	   PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO EXPORT. CEF","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA04
           EXP_EPIS()
           RESTORE SCREEN FROM TELA04
	ELSE
	   PLQUA("01")
	   PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO EXPORT. PIS/COFINS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA04
           MENU_REG()
           RESTORE SCREEN FROM TELA04
	ELSE
	   PLQUA("01")
	   PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO.- PIS/COFINS","ACESSO NEGADO")
	ENDIF
     CASE LASTKEY() = 27
	CLOSE DATABASES
        RESTORE SCREEN FROM TELA03
	RETURN NIL
  ENDCASE
ENDDO
*************************  fim  *********************************
