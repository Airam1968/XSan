*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_GPA.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO MENUTENCAO DE PAGAMENTOS BALCAO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_GPA()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU PAGAMENTOS BALCAO")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 16,70
   @ 11,50 TO 16,70
   @ 12,51 PROMPT " 1    Pagamento    " MESSAGE " Pagamento de Parcelas de Financiamento       "
   @ 13,51 PROMPT " 2    Consultas    " MESSAGE " Menu de Consulta a Pagamentos                "
   @ 14,51 PROMPT " 3   Relatorios    " MESSAGE " Menu de Relatórios de Pagamentos             "
   @ 15,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Operação do Módulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("CADASTRO")
           CAD_PAGB()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("CONSULTAS")
           MEN_CPGB()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("RELATORIOS")
           MEN_RPGB()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************




