*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_REL.PRG
* Data        : 01/01/99
* Objetivo    : MENU RELATORIOS GERAIS DO SISTEMA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_REL()
DO WHILE .T.
   dbcloseall()
   PTIT( "MENU RELAT‡RIOS GERAIS 1")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 06,50 CLEAR TO 21,70
   @ 06,50 TO 21,70
   @ 07,51 PROMPT " 1 Extrato Financ. " MESSAGE " Extrato do Financiamento - Individual         "
   @ 08,51 PROMPT " 2 Extrato Correá∆o" MESSAGE " Extrato do Correá∆o do Contrato               "
   @ 09,51 PROMPT " 3     Vendas      " MESSAGE " Relat¢rio de Vendas no Per°odo                "
   @ 10,51 PROMPT " 4    Contabil     " MESSAGE " Relat¢rio Cont·bil Mensal                     "
   @ 11,51 PROMPT " 5   Recebimentos  " MESSAGE " Relat¢rio de Recebimentos no Per°odo          "
   @ 12,51 PROMPT " 6   Devedores     " MESSAGE " Relat¢rio de Devedores no Per°odo             "
   @ 13,51 PROMPT " 7  Inadimplentes  " MESSAGE " Relat¢rio de Inadimplentes no Per°odo         "
   @ 14,51 PROMPT " 8  Saldo Devedor  " MESSAGE " Relat¢rio de Saldo Devedor Acumulado          "
   @ 15,51 PROMPT " 9  Saldo Credor   " MESSAGE " Relat¢rio de Saldo Credor Acumulado           "
   @ 16,51 PROMPT "10    Cobranáa     " MESSAGE " Relat¢rio de Apoio a Cobranáa Externa         "
   @ 17,51 PROMPT "11 Comissao Aberto " MESSAGE " Relat¢rio de Fechamento de Comissoes - A Pagar" 
   @ 18,51 PROMPT "12  Comissao Paga  " MESSAGE " Relat¢rio de Fechamento de Comissoes - Pago   "     
   @ 19,51 PROMPT "13 Devedor Corrig. " MESSAGE " Relat¢rio de Devedores no PerÌodo Corrigido   "
   @ 20,51 PROMPT " Esc    Fim        " MESSAGE " Fim de Operaá∆o do M¢dulo                     "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
      CASE sisOP = 1
         IF F_ACESSO("DI_RE_04")
            REL_EXTR()
               dbcloseall()
            CLOSE DATABASES
         ELSE
            PLQUA("01")
            PMSGR( "Acesso Negado !",1)
            F_LACESSO(sisUSUARIO,"RELATORIO EXTR. FINANCIAMENTO","ACESSO NEGADO")                                         
         ENDIF
      CASE sisOP = 2
         IF F_ACESSO("DI_RE_04")
            REL_EXCO()
               dbcloseall()
            CLOSE DATABASES
         ELSE
            PLQUA("01")
            PMSGR( "Acesso Negado !",1)
            F_LACESSO(sisUSUARIO,"RELATORIO EXTR. CORRECAO","ACESSO NEGADO")                                         
         ENDIF
      CASE sisOP = 3
         IF F_ACESSO("DI_RE_04")
            REL_VEND()
               dbcloseall()
            CLOSE DATABASES
         ELSE
            PLQUA("01")
            PMSGR( "Acesso Negado !",1)
            F_LACESSO(sisUSUARIO,"RELATORIO DE VENDAS NO PERIODO","ACESSO NEGADO")                                
         ENDIF
      CASE sisOP = 4
        IF F_ACESSO("DI_RE_04")
           REL_CONT()
              dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE CONTABILIZACAO","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_04")
           REL_PAGA()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE PAGAMENTOS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_RE_04")
           REL_DEVE()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE DEVEDORES","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_RE_04")
           REL_INAD()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE INADIMPLENTES","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 8
        IF F_ACESSO("DI_RE_04")
           REL_SDEV()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE SALDO DEVEDOR","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 9
        IF F_ACESSO("DI_RE_04")
           REL_SCRE()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE SALDO CREDOR","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 10
        IF F_ACESSO("DI_RE_04")
           REL_AIRA()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE COBRANCA","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 11
        IF F_ACESSO("DI_RE_04")
           REL_COMI()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE COMISSOES - EM ABERTO","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 12
        IF F_ACESSO("DI_RE_04")
           REL_COMP()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE COMISSOES - PAGAS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 13
        IF F_ACESSO("DI_RE_04")
           REL_DEV2()
              dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE DEVEDORES CORRIGIDO","ACESSO NEGADO")                                
        ENDIF		
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA01
ENDDO
*************************  fim  *********************************
