*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_MAL.PRG
* Data        : 01/01/99
* Objetivo    : MENU MALA DIRETA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_MAL()
dbcloseall()
SAVE SCREEN TO TELA03
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU MALA DIRETA")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,49 CLEAR TO 21,70
   @ 11,49 TO 21,70
   @ 12,51 PROMPT " 1  Seleá∆o Nomes " MESSAGE " Seleá∆o para Impressao de Mala Direta ou Carta "
   @ 13,51 PROMPT " 2 Aniversariantes" MESSAGE " Seleá∆o para Impressao de Aniversariantes	"
   @ 14,51 PROMPT " 3    Devedores   " MESSAGE " Seleá∆o para Impressao de Devedores		"
   @ 15,51 PROMPT " 4 Empreendimentos" MESSAGE " Seleá∆o para Impr. de Clientes Empreendimentos "
   @ 16,51 PROMPT " 5    Consultas   " MESSAGE " Consulta de Nomes Selecionados                 "
   @ 17,51 PROMPT " 6   Emitir Mala  " MESSAGE " Emissao da Mala Direta                         "
   @ 18,51 PROMPT " 7 Seleá∆o Cartas " MESSAGE " Seleá∆o da Carta Padr∆o a ser Impressa		"
   @ 19,51 PROMPT " 8  Emitir Cartas " MESSAGE " Emissao da Carta Padr∆o                        "
   @ 20,51 PROMPT " Esc     Fim      " MESSAGE " Fim de Operaá∆o do M¢dulo                      "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA03
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_AL_01")
		   SEL_DIRE()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"SELECAO NOMES DE CLIENTES","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_AL_01")
		   SEL_ANIV()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"SELECAO DE ANIVERSARIANTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_AL_01")
		   SEL_DEVE()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"SELECAO DE DEVEDORES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_AL_01")
		   SEL_CLIE()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"SELECAO CLIENTES EMPREENDIMENTOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CO_01")
		   CON_NOME()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE NOMES SELECIONADOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_RE_01")
		   MAL_DIRE()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"EMISSAO DE MALA DIRETA","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_AL_01")
		   SEL_CART()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"SELECAO DE CARTAS PADRAO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 8
        IF F_ACESSO("DI_RE_01")
		   MAL_CART()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"EMISSAO DE CARTA PADRAO","ACESSO NEGADO")        
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA03
		RETURN nil
  ENDCASE
  RESTORE SCREEN FROM TELA03
ENDDO
*************************  fim  *********************************
