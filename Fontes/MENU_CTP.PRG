*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CTP.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO CONTAS A PAGAR
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MENU_CTP()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT("MENU CONTAS A PAGAR")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 09,39 CLEAR TO 20,60
   @ 09,39 TO 20,60
   @ 10,41 PROMPT " 1   Cadastro     " MESSAGE " Cadastro de Contas a Pagar                "
   @ 11,41 PROMPT " 2   Alteraá∆o    " MESSAGE " Alteraá∆o de Contas a Pagar               "
   @ 12,41 PROMPT " 3   Exclus∆o     " MESSAGE " Exclus∆o de Contas a Pagar                "
   @ 13,41 PROMPT " 4 Baixa Lanáamen." MESSAGE " Baixa de Contas a Pagar                   "   
   @ 14,41 PROMPT " 5 Cancel. Baixa  " MESSAGE " Cancelamento de Baixa de Contas a Pagar   "   
   @ 15,41 PROMPT " 6   Consultas    " MESSAGE " Consultas de Contas a Pagar               "
   @ 16,41 PROMPT " 7   Relat¢rio    " MESSAGE " Emiss∆o do Relat¢rio de CPagar            "
   @ 17,41 PROMPT " 8    Tabelas     " MESSAGE " Menu Manutená∆o de Tabelas                "
   @ 18,41 PROMPT " 9   Importaá∆o   " MESSAGE " Importaá∆o de Lanáamentos                 "   
   @ 19,41 PROMPT " Esc     Fim      " MESSAGE " Fim de Operaá∆o do M¢dulo                 "
   READ
   sisOP=1
   MENU TO sisOP
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CA_05")
           CAD_CTPG()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CADASTRO DE CONTAS A PAGAR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_AL_05")
           ALT_CTPG()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"ALTERACAO DE CONTAS A PAGAR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_EX_05")
           EXC_CTPG()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"EXCLUSAO DE CONTAS A PAGAR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 4
        IF F_ACESSO("DI_AL_05")
           BAI_CTPG()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"BAIXA DE CONTAS A PAGAR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 5
        IF F_ACESSO("DI_AL_05")
           CAN_CTPG()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CANC. BAIXA DE CONTAS A PAGAR","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 6
        MENU_CPG()
     CASE sisOP = 7
        MENU_RPG()
     CASE sisOP = 8
        MENU_TCP()
     CASE sisOP = 9
        IMP_CTPG()  
     CASE LASTKEY() = 27
        CLOSE DATABASES
        dbcloseall()        
        RESTORE SCREEN FROM TELA04
	RETURN nil
  ENDCASE
ENDDO
RETURN nil
*************************  fim  *********************************
