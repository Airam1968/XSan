*********************************************************************
* Sistema     : Gestao de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : TAB_TCLA.PRG
* Data        : 01/01/99
* Objetivo    : TABELA DE CLASSE DE PRODUTO
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
function TAB_TCLA()
SAVE SCREEN TO TELA03
PTIT( "TABELA DE PRODUTOS")
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   SELE 3
   USE CONTAS INDEX CONTAS
   GOTO TOP
   SELE 2
   USE TIPO_PRO INDEX TIPO_PRO ALIAS S2
   GOTO TOP
   SELE 1
   USE CLAS_PRO INDEX CLAS_PRO ALIAS S1
   GOTO TOP
   SET RELATION TO S1->TIPO INTO S2
   GOTO TOP
   PLIMPA()
   SET COLOR TO W/N,+W/R
   DECLARE CAMPO[16],PIC[16],CAB[16],LENS[16]
   AFIELDS(CAMPO,.T.,LENS)
   CAMPO[2]="S2->DESCRICAO"
   LENS[2] =20
   PIC[1] ="@R99999"
   PIC[2] ="@S20"
   PIC[3] ="@S40"
   PIC[4] ="@R99999"
   PIC[5] ="@S50"
   PIC[6] ="@S20"
   PIC[7] ="@S20"
   PIC[8] ="@S2"
   PIC[9] ="@R 99999-999"
   PIC[10]="@R 999,999,999.99"
   PIC[11]="@S1"
   PIC[12]="99/99/9999"
   PIC[13]="@R 999,999,999.99"
   PIC[14]="@S20"
   PIC[15]="@S20"
   PIC[16]="@S4"   
   CAB[1] ="CODIGO"
   CAB[2] ="TIPO"
   CAB[3] ="NOME"
   CAB[4] ="COD. CONTA"
   CAB[5] ="ENDERECO DO PRODUTO"
   CAB[6] ="BAIRRO"
   CAB[7] ="CIDADE"
   CAB[8] ="ESTADO"
   CAB[9] ="CEP"
   CAB[10]="M2 TOTAL"
   CAB[11]="SITUACAO"
   CAB[12]="HABITE-SE"
   CAB[13]="CUSTO BASE PREVISTO"
   CAB[14]="OBSERVACOES"
   CAB[15]="OBSERVACOES"
   CAB[16]="C…DIGO PIS"   
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
   @ 23,65 SAY "Movimentacao"
   DBEDIT(06,01,21,78,CAMPO,"F_CAD_TCLA",PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
************************* funcao *********************************
FUNCTION F_CAD_TCLA
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
	    eCOD_CONTA = 0
	    DO WHILE .T.
	       @ 10,10 CLEAR TO 18,70
	       @ 10,10 TO 18,70 DOUBLE
	       PCENTRA( "Dados do Produto",11)
	       eCODIGO    = 0
	       eTIPO      = 0
	       eDESCRICAO = SPACE(40)
               @ 13,17 SAY "Codigo   : " GET eCODIGO    PICT "999"
	       READ
	       SEEK eCODIGO
	       IF FOUND()
		  PLQUA("01")
		  PMSGR( "Codigo ja Cadastrado !",3)
		  LOOP
	       ENDIF
	       DO WHILE .T.
		  @ 14,17 SAY "Tipo     : " GET eTIPO      PICT "99999"
		  READ
		  SELE 2
		  SEEK eTIPO
		  IF .NOT. FOUND()
		     PLQUA("01") 
		     PMSGR( "Tipo Nao Encontrado !",3)
		     SELE 1
		     LOOP
		  ELSE
		     @ 14,37 SAY " - "+ALLTRIM(DESCRICAO)
		     SELE 1
		     EXIT
		  ENDIF
	       ENDDO
	       @ 15,17 SAY "Descricao: " GET eDESCRICAO   PICT "@S40"
	       READ
	       DO WHILE .T.
		  @ 16,17 SAY "Cod.Conta: " GET eCOD_CONTA   PICT "@r 99999"
		  READ
		  IF eCOD_CONTA = 0
		     PLQUA("01")
		     PMSGR( "Aten‡Æo !",3)
		     PPERG ( "Codigo de Conta Nao Fornecido, Confirma (S/N) ? ")
		     IF sisRESP = "S"
			EXIT
		     ELSE
			LOOP
		     ENDIF
		  ELSE
		     SELE 3
		     SEEK eCOD_CONTA
		     IF FOUND()
			SELE 1
			EXIT
		     ELSE
			PLQUA("01")
			PMSGR( "Aten‡Æo !",3)
			PMSGR( "C½digo Nao Cadastrado !",3)
			SELE 1
			LOOP
		     ENDIF
		  ENDIF
	       ENDDO
	       PLQUA("01")
	       PPERG ( "Confirma (S/N) ? ")
	       IF sisRESP = "S"
		  SELE 1
		  APPEND BLANK
		  IF F_BLOQR()
		     REPLACE CODIGO     WITH eCODIGO,;
			     DESCRICAO  WITH eDESCRICAO,;
			     TIPO       WITH eTIPO
		     REPLACE COD_CONTA  WITH eCOD_CONTA
		     UNLOCK
		  ENDIF
		  EXIT
	       ELSE
		  EXIT
	       ENDIF
	    ENDDO
	 ENDIF
	 SET CURSOR OFF
	 RESTORE SCREEN
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
		   
