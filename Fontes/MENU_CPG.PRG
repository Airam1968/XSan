*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CPG.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CONSULTAS DO CONTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_CPG()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU CONSULTAS DO CONTAS A PAGAR")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 21,69
   @ 11,50 TO 21,69
   @ 12,51 PROMPT " 1     Geral      " MESSAGE " Consulta de T¡tulos por Per‹¨«odo do        "
   @ 13,51 PROMPT " 2 Tit. em Aberto " MESSAGE " Consulta de T¡tulos em Aberto             "
   @ 14,51 PROMPT " 3 Tit. Liquidados" MESSAGE " Consulta de T¡tulos Liquidados            "
   @ 15,51 PROMPT " 4   Por Classe   " MESSAGE " Consulta de T¡tulos de Classe de Despesa  "
   @ 16,51 PROMPT " 5 Por Fornecedor " MESSAGE " Consulta de T¡tulos por Fornecedor        "
   @ 17,51 PROMPT " 6 Produto/Classe " MESSAGE " Consulta por Empreendimento e Classe Desp."
   @ 18,51 PROMPT " 7 Produto/Fornec." MESSAGE " Consulta por Empreendimento e Fornecedor  "
   @ 19,51 PROMPT " 8     Cheques    " MESSAGE " Pesquisa de Cheque Emitido                "
   @ 20,51 PROMPT " Esc     Fim      " MESSAGE " Fim de Opera‡Æo do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CO_05")
           CON_CTPG()
           dbcloseall()           
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - GERAL","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CO_05")
           CON_CTPA()
           dbcloseall()           
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - T.ABERTO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CO_05")
           CON_CTPB()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - BAIXAS","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CO_05")
           CON_CTPC()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - CLASSE","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CO_05")
           CON_CTPF()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - FORNECEDOR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6     
        IF F_ACESSO("DI_CO_05")
           CON_CTPE()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR -    PRODUTO    ","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_CO_05")
           CON_CTP2()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR -     PRODUTO    ","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 8
        IF F_ACESSO("DI_CO_05")
           CON_CHEQ()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CPAGAR - CHEQUES","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RESTORE SCREEN FROM TELA02
        RETURN nil
  ENDCASE
ENDDO
*************************  fim  *********************************
