*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_PAG.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO MANUTENCAO DE VENDAS/PAGAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_PAG()
dbcloseall()
SAVE SCREEN TO TELA02
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU VENDAS/PAGAMENTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,40 CLEAR TO 20,60
   @ 09,40 TO 20,60
   @ 10,41 PROMPT " 1      Venda      " MESSAGE " Rotina de Venda de Unidades Imobili rias       "
   @ 11,41 PROMPT " 2   Cancelamento  " MESSAGE " Rotina de Cancelamento de Venda                "
   @ 12,41 PROMPT " 3    Parcelas     " MESSAGE " Menu de Manuten‡Æo de Parcelas Financimento    "
   @ 13,41 PROMPT " 4  Cobr. Banc ria " MESSAGE " Menu de Automatiza‡Æo de Cobran‡a Banc ria     "
   @ 14,41 PROMPT " 5   Consultas     " MESSAGE " Menu de Consultas a Pagamentos                 "
   @ 15,41 PROMPT " 6   Relat¢rios 1  " MESSAGE " Menu EmissÆo de Relat¢rios 1                   "
   @ 16,41 PROMPT " 7   Relat¢rios 2  " MESSAGE " Menu EmissÆo de Relat¢rios 2                   "
   @ 17,41 PROMPT " 8  Rot. Especiais " MESSAGE " Menu Rotinas Especiais                         "
   @ 18,41 PROMPT " 9     Tabelas     " MESSAGE " Menu de Manutencao de Tabelas de Pagamentos    "
   @ 19,41 PROMPT " Esc     Fim       " MESSAGE " Fim de Opera‡Æo do M¢dulo                      "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA02
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_03")
           VEN_IMOV()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CADASTRO DE VENDA","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_EX_03")
           EXC_IMOV()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"EXCLUSAO DE VENDA","ACESSO NEGADO")        
        ENDIF
     CASE sisOP = 3
        MENU_PGB()
     CASE sisOP = 4
        MENU_COB()
     CASE sisOP = 5
        MENU_CPA()
     CASE sisOP = 6
        MENU_REL()
     CASE sisOP = 7
        MENU_RPA()
     CASE sisOP = 8
        MENU_RNG()
     CASE sisOP = 9
        MENU_TPA()
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA02
ENDDO
RETURN nil
*************************  fim  *********************************
