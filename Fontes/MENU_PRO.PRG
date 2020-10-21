*********************************************************************
* Sistema     : Gest‹¨«‹¨«‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_PRO.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO MANUTENCAO DE PRODUTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_PRO()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU PRODUTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 17,60
   @ 09,40 TO 17,60
   @ 10,41 PROMPT " 1    InclusÆo     " MESSAGE " InclusÆo de Produtos para Venda              "
   @ 11,41 PROMPT " 2    Altera‡Æo    " MESSAGE " Altera‡Æo de Produtos                        "
   @ 12,41 PROMPT " 3    ExclusÆo     " MESSAGE " ExclusÆo de Produtos                         "
   @ 13,41 PROMPT " 4    Consultas    " MESSAGE " Menu de Consultas a Produtos                 "
   @ 14,41 PROMPT " 5   Relat¢rios    " MESSAGE " Menu EmissÆo de Relat¢rios                   "
   @ 15,41 PROMPT " 6    Tabelas      " MESSAGE " Menu Tabelas de Informa‡äes de Produtos      "
   @ 16,41 PROMPT " Esc     Fim       " MESSAGE " Fim de Opera‡Æo do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_02")
           INC_PROD()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CADASTRO DE PRODUTOS","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_AL_02")
           ALT_PROD()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ALTERACAO DE PRODUTOS","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_EX_02")
           EXC_PROD()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"EXCLUSAO DE PRODUTOS","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 4
        MENU_CPR()
     CASE sisOP = 5
        MENU_RPR()
     CASE sisOP = 6
        MENU_TPR()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA01
ENDDO
RETURN nil
*************************  fim  *********************************
