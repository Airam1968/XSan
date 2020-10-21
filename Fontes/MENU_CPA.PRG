*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CPA.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO CONSULTA DE VENDA/PAGAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_CPA()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU CONSULTA DE VENDA/PAGAMENTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 06,50 CLEAR TO 20,70
   @ 06,50 TO 20,70
   @ 07,51 PROMPT " 1  Consulta Venda " MESSAGE " Consulta de Dados da Venda                   "   
   @ 08,51 PROMPT " 2   Proprietario  " MESSAGE " Consulta de Unidades por Proprietario        "
   @ 09,51 PROMPT " 3   Vend./Disp.   " MESSAGE " Consulta de Unidades Vendidas/Disponiveis    "
   @ 10,51 PROMPT " 4    Quitados     " MESSAGE " Consulta de Unidades Quitadas                "
   @ 11,51 PROMPT " 5   Em Processo   " MESSAGE " Consulta de Unidades em Processo Judicial    "
   @ 12,51 PROMPT " 6  Fluxo Parcelas " MESSAGE " Consulta de Parcelas do Financiamento        "
   @ 13,51 PROMPT " 7    Pagamentos   " MESSAGE " Consulta de Pagamentos no Per¡odo            "
   @ 14,51 PROMPT " 8    Devedores    " MESSAGE " Consulta de Devedores no Per¡odo             "
   @ 15,51 PROMPT " 9   Inadimplentes " MESSAGE " Consulta de Totais de Inadimplentes          "
   @ 16,51 PROMPT " 10 Financiamentos " MESSAGE " Consulta de Status dos Financiamentos        "
   @ 17,51 PROMPT " 11 Comiss. Aberto " MESSAGE " Consulta de Totais de Comissäes em Aberto    "
   @ 18,51 PROMPT " 12 Comiss. Pagas  " MESSAGE " Consulta de Totais de Comissäes Pagas        "
   @ 19,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Opera‡Æo do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CO_03")
           CON_IMOV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE VENDAS","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CO_03")
           CON_PRPR()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE PROPRIETARIO","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CO_03")
           CON_PROV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE VENDIDOS\DISPONIVEIS","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CO_03")
           CON_PROQ()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE QUITADOS","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CO_03")
           CON_PROP()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA POR PROCESSO","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_CO_03")
           CON_PAGP()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE FLUXO DE PARCELAS","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_CO_03")
           CON_PAGO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE PAGAMENTOS 1","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 8
        IF F_ACESSO("DI_CO_03")
           CON_DEVE()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE DEVEDORES 1","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 9
        IF F_ACESSO("DI_CO_03")
           CON_INAD()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA INADIMPLENTES","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 10
        IF F_ACESSO("DI_CO_03")
            CON_FINA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA STATUS FINANCIAMENTOS","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 11
        IF F_ACESSO("DI_CO_03")
           CON_COMI()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA COMISSAO EM ABERTO","ACESSO NEGADO")                        
        ENDIF
     CASE sisOP = 12
        IF F_ACESSO("DI_CO_03")
            CON_COMP()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA COMISSAO PAGA","ACESSO NEGADO")                        
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************
