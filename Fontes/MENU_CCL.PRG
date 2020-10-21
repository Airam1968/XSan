*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CCL.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE CONSULTA DE CLIENTES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_CCL()
DO WHILE .T.
   dbcloseall()
   PTIT("MENU CONSULTA DE CLIENTES")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 16,70
   @ 11,50 TO 16,70
   @ 12,51 PROMPT " 1  Consulta Nome  " MESSAGE " Consulta de Clientes por Nome                "
   @ 13,51 PROMPT " 2  Consulta Cod.  " MESSAGE " Consulta de Clientes por Codigo              "   
   @ 14,51 PROMPT " 3     Listagem    " MESSAGE " Consulta a Listagem de Clientes              "
   @ 15,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Opera‡Æo do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CO_01")
           CON_CLIN()
           dbcloseall()
           CLOSE DATABASES
        ELSE
           PLQUA("01")
		   PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CLIENTE NOME","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 2
        IF F_ACESSO("DI_CO_01")
           CON_CLIC()
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CLIENTE CODIGO","ACESSO NEGADO")
        ENDIF
     CASE sisOP = 3
        IF F_ACESSO("DI_CO_01")
           CON_CLIL()
           dbcloseall()
        ELSE
           PLQUA("01")
           PMSGR("Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA CLIENTE LISTAGEM","ACESSO NEGADO")
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
		RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************
