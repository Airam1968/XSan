*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_RCM.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE RELATORIOS DE COTACOES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_RCM()
SAVE SCREEN TO TELA03
DO WHILE .T.
   PTIT( "MENU RELATORIOS COTAÄÂES")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 20,70
   @ 11,50 TO 20,70
   @ 12,51 PROMPT " 1 Pedido Cotaá∆o  " MESSAGE " Emissao de Relat¢rio de Pedido de Cotaá∆o    "
   @ 13,51 PROMPT " 2     Cotaá∆o     " MESSAGE " Emiss∆o de Cotaá∆o                           "
   @ 14,51 PROMPT " 3  Ped. Compra 1  " MESSAGE " Emiss∆o de Pedidos de Compra - Modelo 1      "
   @ 15,51 PROMPT " 4  Ped. Compra 2  " MESSAGE " Emiss∆o de Pedidos de Compra - Modelo 2      "
   @ 16,51 PROMPT " 5   Fornecedores  " MESSAGE " Emiss∆o de Cadastro de Fornecedores          "
   @ 17,51 PROMPT " 6      Tipo       " MESSAGE " Emiss∆o de Cadastro de Tipo de Produtos      "
   @ 18,51 PROMPT " 7      Itens      " MESSAGE " Emiss∆o de Cadastro de Produtos              "
   @ 19,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA03
   DO CASE
     CASE sisOP = 1
		REL_PECO()
     CASE sisOP = 2
		REL_COTA()
     CASE sisOP = 3
		REL_PEDC()
     CASE sisOP = 4
		REL_PDC2()
     CASE sisOP = 5
		REL_FORN()
     CASE sisOP = 6
		REL_TIPO()
     CASE sisOP = 7
		REL_ITEM()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA03
ENDDO
*************************  fim  *********************************
