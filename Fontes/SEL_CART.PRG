*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : SEL_CART.PRG
* Data        : 01/01/99
* Objetivo    : Selecao de Carta Padrao
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION SEL_CART()
SAVE SCREEN TO TELA03
PTIT("SELECAO DE CARTA PADRÇO")
SELE 1
USE CAR_PADR INDEX CAR_PADR
GOTO TOP
*PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[1],PIC[1],CAB[1],LENS[1]
CAMPO[1] = "NOME"
LENS[1]  = 10
PIC[1]="XXXXXXXXXX"
CAB[1]="  CARTA"
@ 6,65 TO 19,76 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W+,I,N,N,I
@ 23,00 SAY "[ENTER]"
@ 23,21 SAY "[ESC]"
@ 23,37 SAY "[ESPA‡O]"
@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,07 SAY "Para Alterar"
@ 23,26 SAY "Para Sair"
@ 23,45 SAY "Para Incluir"
@ 23,65 SAY "Movimenta‡Æo"
DBEDIT(7,66,19,75,CAMPO,"KEYS",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION KEYS
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 32
         SAVE SCREEN
         SET CURSOR ON
         PLQUA("01")
         PPERG ("Confirma Inclusao (S/N) ? ")
         IF sisRESP = "S"
            eNOME = SPACE(10)
            DO WHILE .T.
               PLQUA("01")
               @ 23,01 SAY "Informe a Nome : " GET eNOME
               READ
               IF eNOME = SPACE(10)
                  PMSGR("Nome Invalido",3)
                  LOOP
               ELSE
                  GOTO TOP
                  SEEK eNOME
                  IF FOUND()
                     PMSGR("Nome ja cadastrado",3)
                     LOOP
                  ELSE
                     APPEND BLANK
		     IF F_BLOQR()
                        REPLACE NOME WITH eNOME
			UNLOCK
		     ENDIF
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
        ENDIF
        SET CURSOR OFF
        RESTORE SCREEN
      CASE KEY == 13
         SAVE SCREEN TO TELA04
         CLEAR
         SET CURSOR ON
	 IF F_BLOQA()
            @ 00,00 TO 24,79 DOUBLE
            @ 02,01 TO 02,78
            @ 01,03 SAY "Carta Padr‡o : "+NOME
            @ 03,01 GET LINHA_1
            @ 04,01 GET LINHA_2
            @ 05,01 GET LINHA_3
            @ 06,01 GET LINHA_4
            @ 07,01 GET LINHA_5
            @ 08,01 GET LINHA_6
            @ 09,01 GET LINHA_7
            @ 10,01 GET LINHA_8
            @ 11,01 GET LINHA_9
            @ 12,01 GET LINHA_10
            @ 13,01 GET LINHA_11
            @ 14,01 GET LINHA_12
            @ 15,01 GET LINHA_13
            @ 16,01 GET LINHA_14
            @ 17,01 GET LINHA_15
            @ 18,01 GET LINHA_16
            @ 19,01 GET LINHA_17
            @ 20,01 GET LINHA_18
            @ 21,01 GET LINHA_19
            @ 22,01 TO 22,78
            READ
            eREGISTRO = RECNO()
            REPLACE ALL SELECAO WITH " "
            GOTO eREGISTRO
            REPLACE SELECAO WITH "X"
	    UNLOCK
	 ENDIF
         SET CURSOR OFF
         RESTORE SCREEN FROM TELA04
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
