*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RPA.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO RELATORIOS DE VENDA/PAGAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RPA()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU RELATORIOS DE VENDA/PAGAMENTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 07,50 CLEAR TO 20,70
   @ 07,50 TO 20,70
   @ 08,51 PROMPT " 1    Vendidos     " MESSAGE " Relatorio Unidades Vendidas                  "
   @ 09,51 PROMPT " 2   Disponiveis   " MESSAGE " Relatorio Unidades Disponiveis               "
   @ 10,51 PROMPT " 3    Quitados     " MESSAGE " Relatorio Unidades Quitadas                  "
   @ 11,51 PROMPT " 4   Em Processo   " MESSAGE " Relatorio Unidades em Proc.Judicial          "
   @ 12,51 PROMPT " 5    Boletos      " MESSAGE " Emissao de Boletos para Pagamento            "
   @ 13,51 PROMPT " 6 NP Form. 80 Col." MESSAGE " Emissao de Notas Promiss¢rias Financiamento  "
   @ 14,51 PROMPT " 7  Renegociaá‰es  " MESSAGE " Emissao de Relat¢rio de Renegociaá‰es        "
   @ 15,51 PROMPT " 8   Desistentes   " MESSAGE " Emissao de Relat¢rio de Desistentes          "
   @ 16,51 PROMPT " 9 Mes de Correá∆o " MESSAGE " Emissao de Relat¢rio de Mes de Correcao Anual"
   @ 17,51 PROMPT "10 Posiá∆o Carteira" MESSAGE " Emissao de Relat¢rio de Posiá∆o de Carteira  "
   @ 18,51 PROMPT "11 Posiá∆o Financi." MESSAGE " Emissao de Relat¢rio de Posiá∆o de Financiam."
   @ 19,51 PROMPT " Esc    Fim        " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_RE_03")
           REL_PROV()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE IMOVEIS VENDIDOS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_RE_03")
           REL_PROD()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE IMOVEIS DISPONIVEIS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_RE_03")
           REL_PROQ()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE IMOVEIS QUITADOS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_RE_03")
           REL_PROP()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE IMOVEIS PROCESSO","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_RE_03")
           MENU_BOL()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
        ENDIF
     CASE sisOP = 6
        IF F_ACESSO("DI_RE_03")
           REL_NPRO()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE NOTAS PROMISSORIAS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 7
        IF F_ACESSO("DI_RE_03")
           REL_RNEG()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE RENEGOCIACOES","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 8
        IF F_ACESSO("DI_RE_03")
           REL_DESI()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE DESISTENTES","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 9 
        IF F_ACESSO("DI_RE_03")
           REL_MESC()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE MES DE CORRECAO","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 10
        IF F_ACESSO("DI_RE_03")
           REL_FINA()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE CARTEIRA","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 11
        IF F_ACESSO("DI_RE_03")
           REL_FIN2()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"RELATORIO DE FINANCIAMENTOS","ACESSO NEGADO")                                
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************
