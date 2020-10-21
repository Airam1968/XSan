*********************************************************************
* Sistema     : Gest∆o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : SEL_CLIE.PRG
* Data        : 01/01/99
* Objetivo    : SELECAO DE CLIENTES POR PRODUTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION SEL_CLIE()
PPERG ("Confirma (S/N) ?")
IF sisRESP = "S"
   PTIT("SELECAO DE CLIENTES POR PRODUTO")
   PLQUA("01")
   eEMPREEND = 0
   @ 23,02 SAY "Informe o  N£mero  do  Produto  ou  (0)  para  todos : " GET eEMPREEND PICT "@R 9999"
   READ
   SELECT 1
   USE CONTATO INDEX NUM_CLIE,CONTATO
   SET FILTER TO NUM_CLIE <> 1 .AND. NUM_CLIE <> 0
   GO TOP
   SKIP
   PLQUA("01")
   PPERG("Deseja eliminar seleá∆o anterior (S/N) ? ")
   IF sisRESP = "S"
      IF F_BLOQA()
         REPLACE ALL SELECAO WITH " "
         UNLOCK
      ENDIF
   ENDIF
   PLQUA("01")
   PMENSAG("Aguarde ...")
   SELE 2
   USE UNI_IMOB
   GOTO TOP
   INDEX ON PROPRIET_1 TO UNI_IMO1
   GOTO TOP
   INDEX ON PROPRIET_2 TO UNI_IMO2
   GOTO TOP
   USE UNI_IMOB INDEX UNI_IMO1,UNI_IMO2
   GOTO TOP
   IF eEMPREEND <> 0
      SET FILTER TO CLASSE = STR(eEMPREEND,5)
      GOTO TOP
   ENDIF
   SELE 1
   GOTO TOP
   DO WHILE .NOT. EOF()
      DO WHILE .T. .AND. .NOT. EOF()
         SELE 1
         eNUM_CLIE = NUM_CLIE
         @ 23,15 SAY eNUM_CLIE PICT "@R 999,999"
         SELE 2
         SET ORDER TO 1
         GOTO TOP
         SEEK eNUM_CLIE
         IF .NOT. FOUND()
            SELE 2
            SET ORDER TO 2
            GOTO TOP
            SEEK eNUM_CLIE
            IF .NOT. FOUND()
               SELE 1
               SKIP
               LOOP
            ELSE
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
         SELE 1
      ENDDO
      DO WHILE .T. .AND. .NOT. EOF()
         SELE 2
         IF PROPRIET_1 = eNUM_CLIE 
            SELE 1
            IF F_BLOQR()
               REPLACE SELECAO WITH "X"
               UNLOCK
            ENDIF
            SELE 2
            SKIP
         ELSE
            IF PROPRIET_2 = eNUM_CLIE
               SELE 1
               IF F_BLOQR()
                  REPLACE SELECAO WITH "X"
                  UNLOCK
               ENDIF
               SELE 2
               SKIP
            ENDIF
         ENDIF
         SELE 2
         IF PROPRIET_1 <> eNUM_CLIE .AND. PROPRIET_2 <> eNUM_CLIE
            SELE 1
            SKIP
            EXIT
         ENDIF
         SELE 1
      ENDDO
      SELE 1
   ENDDO
ENDIF
SET DEVICE TO SCREEN
CLOSE ALL
RETURN nil
***************************** fim ***********************************
