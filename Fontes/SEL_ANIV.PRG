*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : SEL_ANIV.PRG
* Data        : 01/01/99
* Objetivo    : SELECAO PARA MALA DIRETA ANIVERSARIANTES
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function SEL_ANIV()
PPERG ("Confirma (S/N) ? ")
IF sisRESP = "N"
   PLQUA("01")
   RETURN nil
ENDIF
SAVE SCREEN TO TELA03
PTIT("SELECAO DE ANIVERSARIANTES")
SELE 1
USE CONTATO INDEX CONTATO,NUM_CLIE
GO TOP
PLQUA("01")
PPERG("Deseja eliminar seleá∆o anterior (S/N) ? ")
IF sisRESP = "S"
   IF F_BLOQA()
      REPLACE ALL SELECAO WITH " "
      UNLOCK
   ENDIF
ENDIF
SELE 1
GOTO TOP
PLQUA("01")
@ 23,00 say "Selecione o mes : "
SET INTE ON
SET COLOR TO
SET COLOR TO W/N,+W/BG
@ 23,18 PROMPT "Janeiro  "
@ 23,18 PROMPT "Fevereiro"
@ 23,18 PROMPT "Marco    "
@ 23,18 PROMPT "Abril    "
@ 23,18 PROMPT "Maio     "
@ 23,18 PROMPT "Junho    "
@ 23,18 PROMPT "Julho    "
@ 23,18 PROMPT "Agosto   "
@ 23,18 PROMPT "Setembro "
@ 23,18 PROMPT "Outubro  "
@ 23,18 PROMPT "Novembro "
@ 23,18 PROMPT "Dezembro "
READ
sisOP=1
MENU TO sisOP
DO CASE
   CASE sisOP = 1
     eMES = 1
   CASE sisOP = 2
     eMES = 2
   CASE sisOP = 3
     eMES = 3
   CASE sisOP = 4
     eMES = 4
   CASE sisOP = 5
     eMES = 5
   CASE sisOP = 6
     eMES = 6
   CASE sisOP = 7
     eMES = 7
   CASE sisOP = 8
     eMES = 8
   CASE sisOP = 9
     eMES = 9
   CASE sisOP = 10
     eMES = 10
   CASE sisOP = 11
     eMES = 11
   CASE sisOP = 12
     eMES = 12
ENDCASE
GOTO TOP
IF F_BLOQA()
   REPLACE ALL SELECAO WITH "X" FOR VAL(SUBSTR(DTOC(NASCIMENTO),4,2)) = eMES
   UNLOCK
ENDIF
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
