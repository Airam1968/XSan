*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_CART.PRG
* Data        : 01/01/99
* Objetivo    : EMISSAO DE CARTOES DE MESAS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function REL_CART()
PPERG ( "Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT( "EMISSAO DE CARTOES DE MESA")
   SELE 1
   USE ARRANJOS INDEX ARRANJOS
   GO TOP
   DO WHILE .T.
      SET COLOR TO W/N,+W/R
      eDATA = CTOD("")
      PLQUA("01")
      @ 23,00 SAY "Data do Evento : " GET eDATA
      READ
      IF eDATA = CTOD("")
         EXIT
      ENDIF
      SEEK eDATA
      IF .NOT. FOUND()
         PMSGR( "Arranjo N∆o existe",3
         LOOP
      ELSE
         PLQUA("01")
         PPERG ( "Imprime Todas (S/N) ? ")
         SELE 3
         USE CONTATO INDEX NUM_CLIE,CONTATO
         GOTO TOP
         SELE 2
         ARQUIVO = sisDADOS+"RE"+SUBSTR(DTOC(eDATA),1,2)+SUBSTR(DTOC(eDATA),4,2)+SUBSTR(DTOC(eDATA),7,2)
         USE &ARQUIVO
         INDEX ON MESA TO &ARQUIVO
         IF sisRESP <> "S"
            SET FILTER TO CLIENTE <> 0
            GOTO TOP
         ENDIF
         PLQUA("01")
         PMENSAG("Possicione as etiquetas na impressora, Tecle [ENTER] para comecar"
         PPARA()
         DO PIMPR
         PLQUA("01")
         PPERG ( "Deseja teste de posicionamento de impressora ? (S/N)"
         IF sisRESP="S"
            SET DEVICE TO PRINT
            @ PROW(),PCOL() SAY sisLETRAC+" "
            LIN = PROW()
            DO WHILE .T.
               @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               LIN = LIN + 1
               @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               LIN = LIN + 1
               @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               LIN = LIN + 1
               @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               LIN = LIN + 1
               @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               LIN = LIN + 1
               @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               LIN = LIN + 1
               @ LIN,00 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               @ LIN,64 SAY "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
               LIN = LIN + 2
               @ LIN,00 SAY sisLETRAN+" "
               SET DEVICE TO SCREEN
               PLQUA("01")
               PPERG ( "Repetir o teste ? (S/N)"
               IF sisRESP="N"
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         CONSTANTE = 53
         PLQUA("01")
         PMENSAG("Aguarde ...")
         eELEMENTO = RECCOUNT()
         eELEMENTO = eELEMENTO * 10
         DECLARE CLIENTE[eELEMENTO],MESA[eELEMENTO]
         * loop de arquivo
         eAUX = 1
         SELE 2
         GOTO TOP
         DO WHILE .NOT. EOF()
            eNUM_CLIE = CLIENTE
            SELE 3
            GOTO TOP
            SEEK eNUM_CLIE
            IF FOUND()
               CLIENTE[eAUX] = CLIENTE
            ELSE
               CLIENTE[eAUX] = SPACE(20)
            ENDIF
            SELE 2
            MESA[eAUX]    = MESA
            eAUX = eAUX + 1
            SKIP
         ENDDO
         eAUX_ANT = eAUX
         CLIENTE[eAUX] = SPACE(50)
         MESA[eAUX]    = 0
         SET DEVICE TO PRINT
         eTOTAL = eAUX - 1
         eAUX = 1
*        LIN = PROW()
*        @ PROW(),PCOL() SAY sisLETRAN+" "
*        LIN = 1
         @ LIN,00 SAY sisLETRAN+" "
         DO WHILE .T.
            @ LIN,00 SAY "BARCELONA ESPETACULOS"
            @ LIN,38 SAY "BARCELONA ESPETACULOS"
            LIN = LIN + 1
            @ LIN,00 SAY " "
            @ LIN,38 SAY " "
            LIN = LIN + 1
            @ LIN,00 SAY " "
            @ LIN,38 SAY " "
            LIN = LIN + 1
            @ LIN,00 SAY CLIENTE[eAUX]
            @ LIN,38 SAY CLIENTE[eAUX+1]
            LIN = LIN + 1
            @ LIN,00 SAY " "
            @ LIN,38 SAY " "
            LIN = LIN + 1
            @ LIN,00 SAY " "
            @ LIN,38 SAY " "
            @ LIN,00 SAY sisLETRAE+" "
            @ LIN,05 SAY "Mesa "+STR(MESA[eAUX])
            @ LIN,04 SAY "Mesa "+STR(MESA[eAUX])
            @ LIN,25 SAY "Mesa "+STR(MESA[eAUX+1])
            @ LIN,25 SAY "Mesa "+STR(MESA[eAUX+1])
            LIN = LIN + 1
            @ LIN,00 SAY " "
            @ LIN,38 SAY " "
            LIN = LIN + 1
            @ LIN,00 SAY " "
            @ LIN,38 SAY " "
            LIN = LIN + 1
            @ LIN,00 SAY " "
            @ LIN,38 SAY " "
            @ LIN,00 SAY sisLETRAN+" "
            eAUX   = eAUX   + 2
            IF eAUX >= eAUX_ANT
               EXIT 
            ENDIF
         ENDDO
      ENDIF
      EXIT
   ENDDO
ENDIF
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RETURN nil
***********************  fim ***********************************
