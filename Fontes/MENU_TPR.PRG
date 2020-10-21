*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_TPR.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE TABELAS DO MODULO DE PRODUTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_TPR()
dbcloseall()
SAVE SCREEN TO TELA03
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU TABELAS DE PRODUTO")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,49 CLEAR TO 18,70
   @ 11,49 TO 18,70
   @ 12,51 PROMPT " 1 Tipo do Produto" MESSAGE " Tabelas de Tipo do Produto                        "
   @ 13,51 PROMPT " 2     Produto    " MESSAGE " Tabelas de Classificacao do Produto               "
   @ 14,51 PROMPT " 3  Grupo Tipo 1  " MESSAGE " Tabelas de Grupo Tipo 1                           "
   @ 15,51 PROMPT " 4  Grupo Tipo 2  " MESSAGE " Tabelas de Grupo Tipo 2                           "
   @ 16,51 PROMPT " 5  Vlr. Unidade  " MESSAGE " Tabelas de Valores de Unidades                    "
   @ 17,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Operação do Módulo                         "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_02")
           TAB_TPRO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA TIPO PRODUTO","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_02")
           TAB_TCLA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA CLASSE PRODUTO","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CA_02")
           TAB_TQUA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA GRUPOS TIPO 1","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CA_02")
           TAB_TAPA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA UNIDADES TIPO 2","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CA_02")
           TAB_TVUN()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA VALOR DAS UNIDADES","ACESSO NEGADO")        
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA03
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
