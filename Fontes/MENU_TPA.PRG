*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_TPA.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO CONSULTA DE TABELAS DE INDICES DE PAGAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_TPA()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU TABELAS DE VENDA/PAGAMENTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 19,70
   @ 11,50 TO 19,70 
   @ 12,51 PROMPT " 1   Indexadores   " MESSAGE " Tabela de Indexadores                        "
   @ 13,51 PROMPT " 2  Taxas Di rias  " MESSAGE " Tabela dos Indexadores com Giro Di rio       "
   @ 14,51 PROMPT " 3  Taxas Mensais  " MESSAGE " Tabela dos Indexadores com Giro Mensal       "   
   @ 15,51 PROMPT " 4 Mensagens Boleto" MESSAGE " Tabela dos Mensagens dos Boletos             "
   @ 16,51 PROMPT " 5    Corretores   " MESSAGE " Tabela dos Corretores                        "
   @ 17,51 PROMPT " 6    Relatorios   " MESSAGE " Menu Emissao de Relatorios                   "
   @ 18,51 PROMPT " Esc    Fim        " MESSAGE " Fim de Operação do Módulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_03")
           DO TAB_INDI
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA DE INDICES","ACESSO NEGADO")                                        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_03")
           DO TAB_TAXD
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA DE TAXAS DIARIAS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CA_03")
           DO TAB_TAXM
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA DE TAXAS MENSAIS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_CA_03")
           DO TAB_MSGR
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA DE MENSAG. BOLETOS","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_CA_03")
           DO TAB_CORR
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"TABELA DE CORRETORES","ACESSO NEGADO")                                
        ENDIF
     CASE sisOP = 6
        DO MENU_RTP
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************

