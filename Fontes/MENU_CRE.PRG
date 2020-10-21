*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CRE.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CONSULTAS DO CONTAS A RECEBER
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_CRE()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU CONSULTAS DO CONTAS A RECEBER")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 19,69
   @ 11,50 TO 19,69
   @ 12,51 PROMPT " 1     Geral      " MESSAGE " Consulta de Contas a Receber              "
   @ 13,51 PROMPT " 2 Cont.em Aberto " MESSAGE " Consulta de Contas em Aberto              "
   @ 14,51 PROMPT " 3 Cont.Liquidadas" MESSAGE " Consulta de Contas Liquidados             "
   @ 15,51 PROMPT " 4   Por Classe   " MESSAGE " Consulta de Contas de Classe de Receita   "
   @ 16,51 PROMPT " 5  Por Clientes  " MESSAGE " Consulta de Contas por Cliente            "
   @ 17,51 PROMPT " 6  Por Produtos  " MESSAGE " Consulta   de  Contas   por   Produto     "
   @ 18,51 PROMPT " Esc     Fim      " MESSAGE " Fim de Opera‡Æo do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CO_06")
           CON_CTRE()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTAS CRECEBER - GERAL","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CO_06")
           CON_CTRA()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTAS CRECEBER - T.ABERTO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CO_06")
           CON_CTRB()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTAS CRECEBER - BAIXADOS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CO_06")
           CON_CTRC()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTAS CRECEBER - CLAS.RECEITAS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CO_06")
           CON_CTRF()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTAS CRECEBER - CLIENTES","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6     
        IF F_ACESSO("DI_CO_06")
           CON_CTRP()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTAS CRECEBER   -  PRODUTOS    ","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
