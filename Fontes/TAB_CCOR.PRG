*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_CCOR.PRG
* Data        : 01/01/99
* Objetivo    : Tabela de Contas Correntes no Sistema
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_CCOR()
SAVE SCREEN TO TELA03
PTIT( "TABELA DE CONTAS CORRENTES")
SELE 1
USE BANCOS INDEX BANCOS
GOTO TOP
SELE 2
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 3
USE MOD_CAIX
GOTO TOP
SELE 2
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
DECLARE CAMPO[15],PIC[15],CAB[15],LENS[15]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 99999"
PIC[2] ="@R 9999"
PIC[3] ="@S10"
PIC[4] ="@S20"
PIC[5] ="@S30"
PIC[6] ="@S30"
PIC[7] ="@S2"
PIC[8] ="@R 999,999,999.99"
PIC[9] ="@R 99999"
PIC[10]="@R 99999"
PIC[11]="@R 99999"
PIC[12]="@R 99999"
PIC[13]="@R 99.999"
PIC[14]="99/99/99"
PIC[15]="@S20"
CAB[1] ="C¢digo"
CAB[2] ="Banco"
CAB[3] ="Ag‡ncia"
CAB[4] ="Conta Corrente"
CAB[5] ="Nome Ag‡ncia"
CAB[6] ="Cidade"
CAB[7] ="Estado"
CAB[8] ="Limite Cr‡dito"
CAB[9] ="Conta Contabil"
CAB[10]="Despesa CPMF"
CAB[11]="Fornecedor CPMF"
CAB[12]="Produto CPMF"
CAB[13]="% CPMF"
CAB[14]="Data CPMF"
CAB[15]="Complemento"
@ 6,00 TO 19,79 DOUBLE
PLQUA("01")
SET COLOR TO
SET COLOR TO W/N+*
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
DBEDIT(7,01,19,78,CAMPO,"F_CCOR",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_CCOR
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
         PPERG ( "Confirma Inclus‡o (S/N) ? ")
         IF sisRESP = "S"
            SAVE SCREEN TO TELA03
            DO WHILE .T.
               eCODIGO   = 0
               eCOD_BAN  = 0
               eCOD_AGE  = SPACE(10)
               eCOD_CCO  = SPACE(20)
               eNOME_AG  = SPACE(30)
               eCIDADE   = SPACE(30)
               eESTADO   = SPACE(2)
               eLIMITE   = 0
               eCONTABIL = 0
               @ 10,05 CLEAR TO 20,75
               @ 10,05 TO 20,75 DOUBLE
               SET COLOR TO W/N,+W/R
			   PCENTRA( "CADASTRO DA CONTA",11)
               DO WHILE .T.
                  @ 13,07 SAY "C¢digo        : " GET eCODIGO  PICT "@R 99"
                  READ
                  IF eCODIGO <> 0
                     SEEK eCODIGO
                     IF FOUND()
                        PLQUA("01")
						PMSGR( "C¢digo de Conta j‡ Cadastrado",3)
                        LOOP
                     ELSE
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
               DO WHILE .T.
                  @ 12,07 SAY "C¢digo Banco  : " GET eCOD_BAN PICT "@R 9999"
                  READ
                  IF eCOD_BAN <> 0
                     SELE 1
                     SEEK eCOD_BAN
                     IF .NOT. FOUND()
                        PLQUA("01")
						PMSGR( "C¢digo de Banco NÆo Cadastrado",3)
                        LOOP
                     ELSE
                        @ 12,30 SAY NOME_BANCO
                        SELE 2
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
               @ 13,07 SAY "C¢digo Ag‡ncia: " GET eCOD_AGE 
               @ 14,07 SAY "C¢digo C/C    : " GET eCOD_CCO                   
               @ 15,07 SAY "Nome Ag‡ncia  : " GET eNOME_AG               
               @ 16,07 SAY "Cidade        : " GET eCIDADE  
               @ 17,07 SAY "Estado        : " GET eESTADO
               @ 18,07 SAY "Limite Cr‡dito: " GET eLIMITE  PICT "@R 999,999,999.99"
               @ 19,07 SAY "Conta Contabil: " GET eCONTABIL PICT "@R 99999"
               READ
               PLQUA("01")
               PPERG ( "Confirma (S/N) ? ")
               IF sisRESP = "S"
                  PMENSAG("GRAVANDO, AGUARDE ...")
                  SELE 2
                  APPEND BLANK
                  IF F_BLOQR()
                     REPLACE CODIGO     WITH eCODIGO,;
                             COD_BANCO  WITH eCOD_BAN,;
                             COD_AGENCI WITH eCOD_AGE
                     REPLACE COD_CC     WITH eCOD_CCO,;
                             NOME_AGENC WITH eNOME_AG,;
                             CIDADE     WITH eCIDADE
                     REPLACE ESTADO     WITH eESTADO,;
                             LIMITE     WITH eLIMITE,;
                             CONTABIL   WITH eCONTABIL
                     UNLOCK
                  ENDIF
                  SELE 3
                  eCONTA = STR(eCODIGO,5)
                  eZERO = "00"
                  eARQ  = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONTA))))+ALLTRIM(eCONTA)
                  eIND1 = "CONTA_"+SUBSTR(eZERO,1,(2-LEN(ALLTRIM(eCONTA))))+ALLTRIM(eCONTA) 
                  COPY TO &eARQ STRUC
                  USE &eARQ
                  INDEX ON DATA TO &eIND1
                  GOTO TOP
                  SELE 2
                  EXIT
               ELSE
                  EXIT
               ENDIF
            ENDDO
            RESTORE SCREEN FROM TELA03
         ENDIF
         SET CURSOR OFF
         RESTORE SCREEN
      CASE KEY == 13
        SAVE SCREEN
        IF F_BLOQR()
           SET CURSOR ON
           FLD_NAME = CAMPO[DB_COLUMN]
           FLD_PIC  = PIC[DB_COLUMN]
           @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
           READ
           SET CURSOR OFF
           UNLOCK
        ENDIF
        RESTORE SCREEN
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
