*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : MENU_BOL.PRG
* Data        : 01/01/99
* Objetivo    : MENU DE EMISSAO DE BOLETOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function MENU_BOL()
SAVE SCREEN TO TELA05
PTIT("MENU DE EMISS«O DE BOLETOS")
SELE 1
USE BANCOS INDEX BANCOS
GOTO TOP
SET COLOR TO W/N,+W/R
DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
AFIELDS(CAMPO,.T.,LENS)
PIC[1] ="@R 9999"
PIC[2] ="@S50"
CAB[1] ="C¢digo Febraban"
CAB[2] ="Nome Instituiá∆o"
DBEDIT(23,01,23,78,CAMPO,"F_BOLE",PIC,CAB," "," "," ")
eCODIGO = COD_BANCO
DO CASE
   CASE eCODIGO = 01
      REL_B001()
   CASE eCODIGO = 08
      REL_B008()
   CASE eCODIGO = 33
      REL_B033()
   CASE eCODIGO = 104
      REL_B104()
   CASE eCODIGO = 151
      REL_B151()
   CASE eCODIGO = 291
      REL_B291()
   CASE eCODIGO = 341
      REL_B341()
   CASE eCODIGO = 353
      REL_B353()
   CASE eCODIGO = 409
      REL_B409()
   CASE eCODIGO = 415
      REL_B409()
   OTHERWISE
      DO PDESENV
ENDCASE
CLOSE ALL
RESTORE SCREEN FROM TELA05
RETURN nil
*************************  fim  *********************************
**************************** funcao *********************************
FUNCTION F_BOLE()
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
