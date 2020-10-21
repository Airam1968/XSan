*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RT1.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS DAS TABELAS DO CONTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_RT1()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELATàRIOS DAS TABELAS DO CPAGAR")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,60 CLEAR TO 18,79
   @ 09,60 TO 18,79
   @ 10,61 PROMPT " 1     Seleção    " MESSAGE " Seleção de Fornecedores para Emissão de Etiquetas    "
   @ 11,61 PROMPT " 2  Emis.Etiqueta " MESSAGE " Emissão de Etiquetas de Mala Direta                  "
   @ 12,61 PROMPT " 3  Seleção Carta " MESSAGE " Seleção de Carta para Emissão                        "
   @ 13,61 PROMPT " 4    Emis.Carta  " MESSAGE " Emissão de Carta Personalizada por Fornecedor        "
   @ 14,61 PROMPT " 5  Fornecedores  " MESSAGE " Relatório de Tabela de Fornecedores do Contas a Pagar"
   @ 15,61 PROMPT " 6  Clas.Despesas " MESSAGE " Relatório de Tabela de Classificação de Despesas     "
   @ 16,61 PROMPT " 7  Clas.Forneced." MESSAGE " Relatório de Tabela de Classificação de Fornecedores "
   @ 17,61 PROMPT " Esc    Fim       " MESSAGE " Fim de Operação do Módulo                            "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_05")
		   SEL_FORN()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"SELECAO DE FORNECEDORES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_05")
		   MAL_DFOR()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - CLAS.DESPESAS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_05")
		   SEL_CART()
		   dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"SELECAO DE CARTAS PADRAO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_05")
		   MAL_FORN()
		   dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"EMISSAO DE CARTA PADRAO","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_05")
		   REL_FORN()
		   dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - FORNECEDORES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_RE_05")
           REL_CLAD()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - CLAS.DESPESAS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_RE_05")
           REL_CLAF()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS CPAGAR - CLAS.FORNECEDORES","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
     dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
