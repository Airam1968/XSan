*********************************************************************
* Sistema     : Gest‹¨«o de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_MEDI.PRG
* Data        : 01/01/99
* Objetivo    : Consulta a banco de Unidades de Medida
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_MEDI()
SAVE SCREEN TO TELA03
PTIT( "CONSULTA A UNIDADES DE MEDIDA")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 1
   USE UNIDADE INDEX UNIDADE
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1] ="@S3"
   PIC[2] ="@S15"
   CAB[1] ="UNIDADE"
   CAB[2] ="DESCRI‹¨«‹¨«O"
   @ 5,20 TO 21,59 DOUBLE
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
   DBEDIT(06,21,21,58,CAMPO,"F_UNID",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
**************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_UNID
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
	    DO WHILE .T.
	       @ 12,10 CLEAR TO 18,70
	       @ 12,10 TO 18,70 DOUBLE
	       PCENTRA( "Dados da Unidade",13
	       eUNIDADE   = SPACE(3)
	       eDESCRICAO = SPACE(15)
	       @ 15,12 SAY "Unidade       : " GET eUNIDADE
	       READ
	       IF eUNIDADE = SPACE(3)
		  EXIT
	       ENDIF
	       SEEK eUNIDADE
	       IF FOUND()
		  PLQUA("01")
		  PMSGR( "Unidade j‹¨« Cadastrada",3
		  EXIT
	       ELSE
		  @ 16,12 SAY "Descri‹¨«‹¨«o     : " GET eDESCRICAO PICT "@S15"
		  READ
		  PLQUA("01")
		  PPERG ( "Confirma (S/N) ? ")
		  IF sisRESP = "S"
		     SELE 1
		     APPEND BLANK
		     REPLACE UNIDADE    WITH eUNIDADE,;
			     DESCRICAO  WITH eDESCRICAO
		  ELSE
		     EXIT
		  ENDIF
	       ENDIF
	    ENDDO
	 ENDIF
	 SET CURSOR OFF
	 RESTORE SCREEN
      CASE KEY == 13
	 IF DB_COLUMN > 1
	    SAVE SCREEN
	    SET CURSOR ON
	    FLD_NAME = CAMPO[DB_COLUMN]
	    FLD_PIC  = PIC[DB_COLUMN]
	    @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
	    READ
	    SET CURSOR OFF
	    RESTORE SCREEN
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
