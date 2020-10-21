*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RTP.PRG
* Data        : 11/10/2001
* Objetivo    : MENU DO RELATORIOS DE TABELAS DE INDICES DE PAGAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RTP()
dbcloseall()
SAVE SCREEN TO TELA05
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU RELATORIOS DAS TABELAS DE VENDA/PAGAMENTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 13,60 CLEAR TO 17,79
   @ 13,60 TO 17,79 
   @ 14,61 PROMPT " 1  Taxas Mensais " MESSAGE " Relatorio da Tabela dos Indexadores Mensal   "   
   @ 15,61 PROMPT " 2  Taxas Acumul. " MESSAGE " Relatorio da Tabela dos Indice Acumulado     "   
   @ 16,61 PROMPT " Esc    Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA05
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_03")
           REL_INDM()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"REL. TABELA INDICES MENSAL","ACESSO NEGADO")                                        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CA_03")
           REL_IND2()
        ELSE
           PLQUA("01")
           PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"REL. TABELA ACUMUL. MENSAL","ACESSO NEGADO")                                        
        ENDIF

     CASE LASTKEY() = 27
        dbcloseall()     
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA05
ENDDO
*************************  fim  *********************************

