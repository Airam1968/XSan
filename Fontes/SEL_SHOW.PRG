*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : SEL_SHOW.PRG
* Data        : 01/01/99
* Objetivo    : SELECAO PARA MALA DIRETA SHOWS JA ACORRIDOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION SEL_SHOW()
PPERG ("Confirma (S/N) ? ")
IF sisRESP = "N"
   PLQUA("01")
   RETURN nil
ENDIF
SAVE SCREEN TO TELA03
PTIT("SELECAO DE CLIENTES DE SHOWS")
SELE 2
USE ARRANJOS INDEX ARRANJOS
GOTO TOP
SELE 1
USE CONTATO INDEX NUM_CLIE,CONTATO
GO TOP
PLQUA("01")
PPERG("Deseja eliminar seleá∆o anterior (S/N) ? ")
IF sisRESP = "S"
   PLQUA("01")
   PMENSAG("Aguarde a Eliminaá∆o ... ")
   REPLACE ALL SELECAO WITH " "
ENDIF
SELE 1
GOTO TOP
DO WHILE .T.
   PLQUA("01")
   eDATA = CTOD("")
   @ 23,00 say "Selecione o Show : " GET eDATA
   READ
   IF eDATA <> CTOD("")
      SELE 2
      GOTO TOP
      SEEK eDATA
      IF .NOT. FOUND()
         PLQUA("01")
         PMSGR("N∆o H† Show na Data.",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ELSE
      EXIT
   ENDIF
ENDDO
SELE 2
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[1] = "DATA"
CAMPO[2] = "EVENTO"
PIC[1]   ="  /  /  "
PIC[2]   ="@S50"
CAB[1]   ="    "
CAB[2]   ="    "
DBEDIT(23,20,23,78,CAMPO,"KEYS_2",PIC,CAB," "," "," ")
eDATA = DATA
PLQUA("01")
PMENSAG("Aguarde a Seleá∆o ... ")
SELE 3
ARQUIVO = "RE"+SUBSTR(DTOC(eDATA),1,2)+SUBSTR(DTOC(eDATA),4,2)+SUBSTR(DTOC(eDATA),7,2)
USE &ARQUIVO
INDEX ON CLIENTE TO &ARQUIVO
GOTO TOP
DO WHILE .NOT. EOF()
   eCLIENTE = CLIENTE
   SELE 1
   SEEK eCLIENTE
   REPLACE SELECAO WITH "X"
   SELE 3
   SKIP
ENDDO
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
