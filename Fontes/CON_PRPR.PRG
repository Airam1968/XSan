*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_PRPR.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE PRODUTOS POR PROPRIETARIOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CON_PRPR()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA DE UNIDADES POR PROPRIETARIO")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
GO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 3
USE CAD_QUAD INDEX CAD_QUAD
GOTO TOP
SELE 4
USE CAD_APAR INDEX CAD_APAR
GOTO TOP
SELE 5
USE CONTATO INDEX CONTATO,NUM_CLIE
GOTO TOP
PLIMPA()
SET COLOR TO W/N,+W/R
eNUM_CLIE = 0
ePESQUISA = SPACE(49)
@ 05,02 SAY "Nome :" GET ePESQUISA PICT "@! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
READ
ePESQUISA = ALLTRIM(ePESQUISA)
SEEK ePESQUISA
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[2]="CPF_CGC"
LENS[2]=14
PIC[1]="@S50"
PIC[2]="@S14"
CAB[1]="  NOME"
CAB[2]="CGC/CPF"
DBEDIT(5,05,5,79,CAMPO,"KEYS_4",PIC,CAB," "," "," ")
eNUM_CLIE = NUM_CLIE
@ 06,02 SAY "Codigo      :" GET eNUM_CLIE PICT "99999"
CLEAR GETS
SELE 1
SET FILTER TO PROPRIET_1 = eNUM_CLIE .OR. PROPRIET_2 = eNUM_CLIE
GOTO TOP
DECLARE CAMPO[8],PIC[8],CAB[8],LENS[8]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[5]="PROPRIET_1"
CAMPO[6]="QUITADO_1"
CAMPO[7]="PROPRIET_2"
CAMPO[8]="QUITADO_2"
LENS[5]=5
LENS[6]=1
LENS[7]=5
LENS[8]=1
PIC[1]="@R99999"
PIC[2]="@R99999"
PIC[3]="@R99999"
PIC[4]="@R99999"
PIC[5]="@R99999"
PIC[6]="@R99999"
CAB[1]=" Produto "
CAB[2]="Grupo"
CAB[3]="Unid"
CAB[4]="Status"
CAB[5]="1o. Proprietario"
CAB[6]="Quitacao 1o."
CAB[7]="2o. Proprietario"
CAB[8]="Quitacao 2o."
@ 07,00 TO 21,79 DOUBLE
DBEDIT(8,01,21,78,CAMPO,"FUNC_PRPR",PIC,CAB,CHR(196),CHR(179),CHR(205))
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
***************************** fim ***********************************
************************ funcao *********************************
FUNCTION FUNC_PRPR
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN = 5
            SAVE SCREEN TO T_PROP1
            ePROPRIET_1 = PROPRIET_1
            SELE 5
            USE CONTATO INDEX NUM_CLIE,CONTATO
            GOTO TOP
            SEEK ePROPRIET_1
            @ 12,13 CLEAR TO 14,67
            @ 12,13 TO       14,67 DOUBLE
			PCENTRA( "Nome do Proprietario No. 1",12)
            @ 13,15 GET CLIENTE
            CLEAR GETS
            USE CONTATO INDEX CONTATO,NUM_CLIE
            SELE 1
            PPARA()
            RESTORE SCREEN FROM T_PROP1
         ELSE
            IF DB_COLUMN = 7
               SAVE SCREEN TO T_PROP2
               ePROPRIET_2 = PROPRIET_2
               SELE 5
               USE CONTATO INDEX NUM_CLIE,CONTATO
               GOTO TOP
               SEEK ePROPRIET_2
               @ 12,13 CLEAR TO 14,67
               @ 12,13 TO       14,67 DOUBLE
			   PCENTRA( "Nome do Proprietario No. 2",12)
               @ 13,15 GET CLIENTE
               CLEAR GETS
               USE CONTATO INDEX CONTATO,NUM_CLIE      
               SELE 1
               PPARA()
               RESTORE SCREEN FROM T_PROP2
            ELSE
               RET = 0
            ENDIF
         ENDIF
   ENDCASE
ENDIF
RETURN RET
*************************  fim  *********************************
