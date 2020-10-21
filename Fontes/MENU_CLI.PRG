*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CLI.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CLIENTES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_CLI()
DO WHILE .T.
   dbcloseall()
   PTIT("MENU CLIENTES")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 18,60
   @ 09,40 TO 18,60
   @ 10,41 PROMPT " 1    Inclus∆o     " MESSAGE " Inclus∆o de Clientes no Cadastro             "
   @ 11,41 PROMPT " 2    Alteraá∆o    " MESSAGE " Alteraá∆o de Clientes no Cadastro            "
   @ 12,41 PROMPT " 3    Exclus∆o     " MESSAGE " Exclus∆o de Clientes no Cadastro             "
   @ 13,41 PROMPT " 4    Consulta     " MESSAGE " Consulta de Cliente por Nome e CPF/CGC       "   
   @ 14,41 PROMPT " 5 Cadastro Resumo " MESSAGE " Emiss∆o de Relat¢rio Cadastro Resumo         "
   @ 15,41 PROMPT " 6 Cadastro Total  " MESSAGE " Emiss∆o de Relat¢rio Cadastro Completo       "
   @ 16,41 PROMPT " 7   Mala Direta   " MESSAGE " Menu Mala Direta                             "
   @ 17,41 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_01")
           INCLUI()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CADASTRO DE CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_AL_01")
           ALTERA()
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ALTERACAO DE CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_EX_01")
           EXCLUI()
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"EXCLUSAO DE CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CO_01")
           MENU_CCL()
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_01")
           REL_CADR()
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIOS DE CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_RE_01")
           REL_CADT()
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 7
        MENU_MAL()
        dbcloseall()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA01
ENDDO
RETURN nil
*************************  fim  *********************************
