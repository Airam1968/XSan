*********************************************************************
* Sistema     : Controle de Reservas
* Analista    : Airam Miranda de Almeida
* Programador : o proprio
*********************************************************************
* Programa    : SEL_DIRE.PRG
* Data        : 28/05/90
* Objetivo    : CONSULTA DE NOMES SELECIONADOS
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
*********************************************************************
DO PPERG WITH 'Confirma (S/N) ? '
IF sisRESP = 'N'
   DO PLQUA WITH '01'
   RETURN
ENDIF
SAVE SCREEN TO TELA03
DO PTIT WITH 'CONSULTA DE NOMES SELECIONADOS'
SELE 1
USE CONTATO INDEX CONTATO,NUM_CLIE
SET FILTER TO SELECAO = 'X'
IF EOF()
   DO PLQUA WITH '01'
   DO PMSGR WITH 'NÆo h  nomes selecionados ! '
   DO PPARA
   DO PLQUA WITH '01'
   RETURN
ENDIF
GO TOP
DO PLQUA WITH '11'
SELE 1
GOTO TOP
@ 6,00 TO 19,79 DOUBLE
DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[3]='NASCIMENTO'
LENS[3]=8
PIC[1]='@S50'
PIC[2]='@S1'
PIC[3]='  /  /  '
CAB[1]='NOME'
CAB[2]='SELECAO'
CAB[3]='NASCIMENTO'
DBEDIT(7,01,19,78,CAMPO,'FUNC_NOM',PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_NOM
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN > 1
            SET CURSOR ON
            FLD_NAME = CAMPO[DB_COLUMN]
            FLD_PIC  = PIC[DB_COLUMN]
            @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
            READ
            SET CURSOR OFF
         ENDIF
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************

