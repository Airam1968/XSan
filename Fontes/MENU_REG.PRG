*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_REG.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO MANUTEN‡AO REGISTROS PIS/COFINS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_REG()
SAVE SCREEN TO TELA05
dbcloseall()
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU MANUTEN€ÇO REGISTROS PIS/COFINS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,60 CLEAR TO 18,79
   @ 09,60 TO 18,79
   @ 10,61 PROMPT " 1 Registro 0000  " MESSAGE " Manuten‡Æo Dados do Registro 0000         "
   @ 11,61 PROMPT " 2 Registro 0100  " MESSAGE " Manuten‡Æo Dados do Registro 0100         "
   @ 12,61 PROMPT " 3 Registro 0110  " MESSAGE " Manuten‡Æo Dados do Registro 0110         "
   @ 13,61 PROMPT " 4 Registro 0140  " MESSAGE " Manuten‡Æo Dados do Registro 0140         "
   @ 14,61 PROMPT " 5 Registro F200  " MESSAGE " Manuten‡Æo Dados do Registro F200         "
   @ 15,61 PROMPT " 6 Registro M200  " MESSAGE " Manuten‡Æo Dados do Registro M200         "
   @ 16,61 PROMPT " 7 Registro 1800  " MESSAGE " Manuten‡Æo Dados do Registro 1800         "
   @ 17,61 PROMPT " Esc     Fim      " MESSAGE " Fim de Opera‡Æo do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA06
           DO TAB_0000
           RESTORE SCREEN FROM TELA06
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO 0000 PIS COFINS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA06
           DO TAB_0100
           RESTORE SCREEN FROM TELA06
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO 100 PIS COFINS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA06
           DO TAB_0110
           RESTORE SCREEN FROM TELA06
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO 110 PIS COFINS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA06
           DO TAB_0140
           RESTORE SCREEN FROM TELA06
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO 140 PIS COFINS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA06
           DO TAB_F200
           RESTORE SCREEN FROM TELA06
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO F200 PIS COFINS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA06
           DO TAB_M200
           RESTORE SCREEN FROM TELA06
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO M200 PIS COFINS","ACESSO NEGADO")
	ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_CA_09")
           SAVE SCREEN TO TELA06
           DO TAB_1800
           RESTORE SCREEN FROM TELA06
	ELSE
	   PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"MANUTENCAO REGISTRO 1800 PIS COFINS","ACESSO NEGADO")
	ENDIF

     CASE LASTKEY() = 27
        dbcloseall()
	CLOSE DATABASES
        RESTORE SCREEN FROM TELA05
	RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
