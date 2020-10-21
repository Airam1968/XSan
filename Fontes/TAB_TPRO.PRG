*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_TPRO.PRG
* Data        : 01/01/99
* Objetivo    : TABELA DE TIPO DE PRODUTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   : 
************************* inicio ************************************
function TAB_TPRO()
SAVE SCREEN TO TELA03
PTIT( "TABELA DE TIPO DE PRODUTO")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 1
   USE TIPO_PRO INDEX TIPO_PRO
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@R99999"
   PIC[2] ="@S40"
   PIC[3] ="@S10"
   PIC[4] ="@S10"
   PIC[5] ="@S10"
   CAB[1] ="CODIGO"
   CAB[2] ="DESCRICAO"
   CAB[3] ="PRODUTO"
   CAB[4] ="GRUPO"
   CAB[5] ="UNIDADE"
   @ 5,00 TO 21,79 DOUBLE
   PLQUA("01")
   SET COLOR TO
   SET COLOR TO W/N+*
*  @ 23,00 SAY "[ENTER]"
   @ 23,21 SAY "[ESC]"
*  @ 23,37 SAY "[ESPA‡O]"
   @ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
   SET COLOR TO
   SET COLOR TO W/N,+W/R
*  @ 23,07 SAY "Para Alterar"
   @ 23,26 SAY "Para Sair"
*  @ 23,45 SAY "Para Incluir"
   @ 23,65 SAY "Movimenta‡Æo"
   DBEDIT(06,01,21,78,CAMPO,"F_CAD_TPRO",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
*************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_CAD_TPRO
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 IF DB_COLUMN > 1
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
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
*** OPCAO RETIRADA
*     CASE KEY == 32
*         SAVE SCREEN
*         SET CURSOR ON
*         PLQUA("01")
*         PPERG ( "Confirma Inclusao (S/N) ? ")
*         IF sisRESP = "S"
*            DO WHILE .T.
*               @ 10,20 CLEAR TO 16,60
*               @ 10,20 TO 16,60 DOUBLE
*               PCENTRA( "Dados do Tipo",11
*               eCODIGO    = 0
*               eDESCRICAO = SPACE(20)
*               @ 13,17 SAY "Codigo   : " GET eCODIGO    PICT "@R99999"
*               @ 14,17 SAY "Descricao: " GET eDESCRICAO PICT "@S20"
*               READ
*               PLQUA("01")
*               PPERG ( "Confirma (S/N) ? ")
*               IF sisRESP = "S"
*                  SELE 1
*                  APPEND BLANK
*                  IF F_BLOQR()
*                     REPLACE CODIGO     WITH eCODIGO,;
*                             DESCRICAO  WITH eDESCRICAO
*                     UNLOCK
*                  ENDIF
*                  EXIT
*               ELSE
*                  EXIT
*               ENDIF
*            ENDDO
*         ENDIF
*         SET CURSOR OFF
*         RESTORE SCREEN
***************************** fim ***********************************                   
