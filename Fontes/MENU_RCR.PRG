*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RCR.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO RELATORIOS DE CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RCR()
   dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELAT‡RIOS DO CONTAS A RECEBER")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 19,69
   @ 11,50 TO 19,69
   @ 12,51 PROMPT " 1     Geral      " MESSAGE " Relat¢rio de Contas por Per°odo           "
   @ 13,51 PROMPT " 2 Cont.em Aberto " MESSAGE " Relat¢rio de Contas em Aberto             "
   @ 14,51 PROMPT " 3 Cont.Liquidados" MESSAGE " Relat¢rio de Contas Liquidados            "
   @ 15,51 PROMPT " 4   Por Classe   " MESSAGE " Relat¢rio de Contas de Classe de Despesa  "
   @ 16,51 PROMPT " 5  Por Cliente   " MESSAGE " Relat¢rio de Contas por Cliente           "
   @ 17,51 PROMPT " 6  Por Produtos  " MESSAGE " Relat¢rio de Contas por Empreendimento    "
   @ 18,51 PROMPT " Esc    Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_06")
           DO REL_CTRG
              dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CRECEBER - GERAL","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_06")
           DO REL_CTRA
           CLOSE DATABASES
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CRECEBER - T.ABERTO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_06")
           DO REL_CTRB
           CLOSE DATABASES
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CRECEBER - BAIXADOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_06")
           DO REL_CTRC
           CLOSE DATABASES
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CRECEBER - CLAS.RECEITAS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_06")
           DO REL_CTRF
           CLOSE DATABASES
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CRECEBER - CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6     
        IF F_ACESSO("DI_RE_06")
           DO REL_CTRP
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO CRECEBER   -   Produtos   ","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************

