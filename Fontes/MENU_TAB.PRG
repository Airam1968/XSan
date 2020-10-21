*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_TAB.PRG
* Data        : 01/01/99
* Objetivo    : MENU TABELAS DO CANTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_TAB()
DO WHILE .T.
   dbcloseall()
   PTIT( "MENU TABELAS DO CANTAS A PAGAR")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 17,70
   @ 11,50 TO 17,70
   @ 12,51 PROMPT " 1  Fornecedores   " MESSAGE " Manutená∆o de Tabela de Fornecedores         "
   @ 13,51 PROMPT " 2 Classe Forneced." MESSAGE " Manutená∆o de Classes de Fornecedores        "
   @ 14,51 PROMPT " 3  Tipo Produto   " MESSAGE " Manutená∆o de Tipo de Produtos               "
   @ 15,51 PROMPT " 4     Itens       " MESSAGE " Manutená∆o de Itens de Compra                "   
   @ 16,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
     CASE sisOP = 1
*       IF F_ACESSO("CADASTRO")
           DO TAB_FORN
*       ELSE
*          PLQUA("01")
*          PMSGR( "Acesso Negado !",1)
*          F_LACESSO(sisUSUARIO,"TABELA DE FORNECEDOR","ACESSO NEGADO")
*       ENDIF
     CASE sisOP = 2
*       IF F_ACESSO("CADASTRO")
           DO TAB_CLAF
*       ELSE
*          PLQUA("01")
*          PMSGR( "Acesso Negado !",1)
*          F_LACESSO(sisUSUARIO,"TAB.CLASSES DE FORNECEDOR","ACESSO NEGADO")
*       ENDIF
     CASE sisOP = 3
*       IF F_ACESSO("CADASTRO")
           DO TAB_TIPO
*       ELSE
*          PLQUA("01")
*          PMSGR( "Acesso Negado !",1)
*          F_LACESSO(sisUSUARIO,"TABELA DE TIPO","ACESSO NEGADO")
*       ENDIF
     CASE sisOP = 4
*       IF F_ACESSO("CADASTRO")
           DO TAB_ITEM
*       ELSE
*          PLQUA("01")
*          PMSGR( "Acesso Negado !",1)
*          F_LACESSO(sisUSUARIO,"TABELA DE ITENS","ACESSO NEGADO")
*       ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA01
ENDDO
*************************  fim  *********************************
