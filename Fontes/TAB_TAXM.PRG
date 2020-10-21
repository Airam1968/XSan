*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_TAXM.PRG
* Data        : 01/01/99
* Objetivo    : Atualizacao da Tabela de Taxas mensal
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_TAXM()
SAVE SCREEN TO TELA03
PTIT( "ATUALIZACAO DA TABELA DE TAXAS MENSAIS")
SELE 2
USE INDICES INDEX INDICES
GOTO TOP
SELE 1
USE TAXA_MES INDEX TAXA_MES
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
AFIELDS(CAMPO,.T.,LENS)
PIC[1]="99"
CAB[1]="MES"
PIC[2]="9999"
CAB[2]="ANO"
eCAMPOS = 0
eCAMPOS = FCOUNT()
eAUX = 3
DO WHILE eAUX <= eCAMPOS
   CAB[eAUX]=FIELD(eAUX)
   SELE 2
   SEEK CAB[eAUX]
   IF TIPO = "TAXA"
      PIC[eAUX]="@R 999.9999"
   ELSE
      PIC[eAUX]="@R 99999.9999"
   ENDIF
   eAUX = eAUX + 1
   SELE 1
ENDDO
@ 5,00 TO 21,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
@ 23,00 SAY "[ENTER]"
@ 23,21 SAY "[ESC]"
@ 23,37 SAY "[ESPA€O]"
@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
SET COLOR TO
SET COLOR TO W/N,+W/R
@ 23,07 SAY "Para Alterar"
@ 23,26 SAY "Para Sair"
@ 23,45 SAY "Para Incluir"
@ 23,65 SAY "Movimenta‡Æo"
DBEDIT(6,01,21,78,CAMPO,"F_TAXM",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_TAXM
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
         PPERG ( "Confirma Inclusao (S/N) ? ")
         IF sisRESP = "S"
            eMES = 0
            eANO = 0
            DO WHILE .T.
               PLQUA("01")
               @ 23,01 SAY "Informe o Mes : " GET eMES PICT "99"
               @ 23,30 SAY "Informe o Ano : " GET eANO PICT "9999"
               READ
               IF eMES = 0 .OR. eANO = 0
				  PMSGR( "Lastro Invalido",3)
                  LOOP
               ELSE
                  GOTO TOP
                  SEEK eANO+eMES
                  IF FOUND()
					 PMSGR( "Periodo ja cadastrado",3)
                     LOOP
                  ELSE
                     APPEND BLANK
                     IF F_BLOQR()
                        eTAM_MES = LENNUM(eMES)
                        eMES = IIF(eTAM_MES = 1,"0"+STR(eMES,1),STR(eMES,2))
                        eANO = STR(eANO,4)
                        REPLACE MES WITH eMES,;
                                ANO WITH eANO
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
         IF F_BLOQR()
            SAVE SCREEN 
            SET CURSOR ON
            FLD_NAME = CAMPO[DB_COLUMN]
            FLD_PIC  = PIC[DB_COLUMN]
            @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
            READ
            SET CURSOR OFF
            UNLOCK
            RESTORE SCREEN
         ENDIF
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
