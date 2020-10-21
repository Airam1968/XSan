*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_PRI.PRG
* Data        : 01/01/99
* Objetivo    : MENU PRINCIPAL
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_PRI()
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   PTIT("MENU PRINCIPAL")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 07,30 TO 18,50
   @ 08,31 PROMPT " 1    Clientes     " MESSAGE "Manutená∆o de Clientes            "
   @ 09,31 PROMPT " 2    Produtos     " MESSAGE "Manutená∆o de Produtos            "
   @ 10,31 PROMPT " 3   Vendas/Pagam. " MESSAGE "Manutená∆o de Vendas/Pagamentos   "
   @ 11,31 PROMPT " 4 Controle Compras" MESSAGE "Controle de Compras e Cotaá‰es    "
   @ 12,31 PROMPT " 5  Contas a Pagar " MESSAGE "Manutená∆o de Contas a Pagar	   "
   @ 13,31 PROMPT " 6 Contas a Receber" MESSAGE "Manutená∆o de Contas a Receber	   "
   @ 14,31 PROMPT " 7   Caixa/Bancos  " MESSAGE "Manutená∆o do Mov. de Caixa/Bancos"
   @ 15,31 PROMPT " 8  Fluxo de Caixa " MESSAGE "Consultas do Fluxo de Caixa	   "
   @ 16,31 PROMPT " 9   Manutená∆o    " MESSAGE "Manutená∆o Geral do Sistema	   "
   @ 17,31 PROMPT " Esc     Fim       " MESSAGE "Fim de Operaá∆o do Sistema	   "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA01
   DO CASE
     CASE sisOP = 1
        MENU_CLI()
     CASE sisOP = 2
        MENU_PRO()
     CASE sisOP = 3
        MENU_PAG()
     CASE sisOP = 4
        MENU_CMP()
     CASE sisOP = 5
        MENU_CTP()
     CASE sisOP = 6
        MENU_CTR()
     CASE sisOP = 7
        MENU_CAI()
     CASE sisOP = 8
        MENU_FCX()
     CASE sisOP = 9
        MENU_MAN()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA01
ENDDO
RETURN nil
*************************  fim  *********************************
