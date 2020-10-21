*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_CPR.PRG
* Data        : 01/01/99
* Objetivo    : MENU DO CONSULTA DE PRODUTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_CPR()
dbcloseall()
SAVE SCREEN TO TELA04
PLQUA("01")
DO WHILE .T.
   SET COLOR TO W/N,+W/R
   PTIT( "MENU CONSULTA DE PRODUTOS")
   SET INTE ON
   SET COLOR TO
   SET COLOR TO W/N,+W/BG
   @ 11,50 CLEAR TO 14,70
   @ 11,50 TO 14,70
   @ 12,51 PROMPT " 1     Produto     " MESSAGE " Consulta de Unidades do Produto              "
   @ 13,51 PROMPT " Esc     Fim       " MESSAGE " Fim de Operaá∆o do M¢dulo                    "
   READ
   sisOP=1
   MENU TO sisOP
   SAVE SCREEN TO TELA04
   DO CASE
     CASE sisOP = 1
        IF F_ACESSO("DI_CO_02")
           DO CON_PROD
        ELSE
           PLQUA("01")
		   PMSGR( "Acesso Negado !",1)
           F_LACESSO(sisUSUARIO,"CONSULTA DE PRODUTOS","ACESSO NEGADO")        
        ENDIF
     CASE LASTKEY() = 27
        dbcloseall()
        CLOSE DATABASES
        RETURN nil
     ENDCASE
     RESTORE SCREEN FROM TELA04
ENDDO
*************************  fim  *********************************

