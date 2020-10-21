*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RPR.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS DE PRODUTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RPR()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELATORIOS DE PRODUTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 17,70
   @ 11,50 TO 17,70
   @ 12,51 PROMPT " 1    Cadastro     " MESSAGE " Emiss∆o de Relat¢rio Cadastro Geral                  "
   @ 13,51 PROMPT " 2    I.P.T.U.     " MESSAGE " Emiss∆o de Relat¢rio Unidades para Prefeitura        "
   @ 14,51 PROMPT " 3    Produtos     " MESSAGE " Emiss∆o de Relat¢rio de Produtos                     "
   @ 15,51 PROMPT " 4 Relaá∆o Clientes" MESSAGE " Emiss∆o de Relat¢rio de Clientes                     "   
   @ 16,51 PROMPT " Esc    Fim        " MESSAGE " Fim de Operaá∆o do M¢dulo                            "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_02")
           REL_PROG()
        ELSE
           PLQUA("01")
		   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE CADASTRO DE PRODUTOS","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_02")
           REL_IPTU()
        ELSE
           PLQUA("01")
		   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE IPTU","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_02")
           REL_EMPR()
        ELSE
           PLQUA("01")
		   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE PRODUTOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_02")
           REL_CLIE()
        ELSE
           PLQUA("01")
	   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CLIENTES POR PRODUTO","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27 
        dbcloseall()     
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************

