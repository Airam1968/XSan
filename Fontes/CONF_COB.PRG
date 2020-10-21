*********************************************************************
* Sistema     : Administraá∆o de Negácios
* Analista    : Airam Miranda de Almeida
* Programador : o proprio
*********************************************************************
* Programa    : CONF_COB.PRG
* Data        : 12/12/93
* Objetivo    : Configuraá∆o da Cobranáa Eletránica           
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CONF_COB()
SAVE SCREEN TO TELA04
PTIT( "CONFIGURACAO COBRANÄA ELETR‚NICA")
SELE 1
USE BANCOS INDEX BANCOS
GOTO TOP
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   DO WHILE .T.
      PLIMPA()
      SET COLOR TO W/N,+W/R
      eCOD_BANCO = 0
      @ 05,02 SAY "Banco   : "
      GO TOP
      DECLARE CAMPO[2],PIC[2],CAB[2],LENS[2]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R9999"
      PIC[2]="@S50"
      CAB[1]="    "
      CAB[2]="    "
      DBEDIT(5,15,5,75,CAMPO,"F_CONF_COB",PIC,CAB," "," - "," ")
      eCOD_BANCO = COD_BANCO
      IF F_BLOQR()
	 REPLACE DATA WITH DATE()
	 COMMIT
         @ 06,05 SAY "Data Configuraá∆o    : " GET DATA
	 CLEAR GETS
         @ 07,05 SAY "Drive de Dados       : " GET DRIVE PICT "@! X"
         @ 08,05 SAY "SubDiret¢rio Remessa : " GET SUBDIR_REM
         @ 09,05 SAY "SubDiret¢rio Retorno : " GET SUBDIR_RET
         @ 10,05 SAY "SubDiret¢rio Envelope: " GET SUBDIR_ENV         
         @ 11,05 SAY "Extens∆o Remessa     : " GET CORI_REM
         @ 11,40 SAY "Extens∆o Retorno     : " GET CORI_RET
         @ 12,05 SAY "Prazo Devoluá∆o Tit. : " GET PRAZO_DEV PICT "@R 999"
         @ 13,05 SAY "Prazo Protesto       : " GET PRAZO_PRO PICT "@R 999"
         @ 14,05 SAY "Prazo Cobranáa Multa : " GET PRAZO_FOL PICT "@R 999"
         @ 15,05 SAY "Multa Por Atraso a.m.: " GET MULTA     PICT "@R 999.99"
         @ 16,05 SAY "Juros a.m.           : " GET JUROS     PICT "@R 999.99"
         @ 17,05 SAY "Comiss∆o Permanencia : " GET COM_PERM  PICT "@R 999.99"
         DO CASE
         CASE eCOD_BANCO = 001
            @ 19,05 SAY "Caixa Postal         : " GET CAIXA_POS
         CASE eCOD_BANCO = 104
            @ 18,05 SAY "Assunto              : " GET ASSUNTO
            @ 19,05 SAY "Caixa Postal         : " GET CAIXA_POS
         CASE eCOD_BANCO = 422
            @ 18,05 SAY "Num. Inicial : " GET NUM_INI
            @ 18,40 SAY "Num. Final   : " GET NUM_FIM
            @ 19,05 SAY "Num. Atual   : " GET NUM_ATU
         ENDCASE
         @ 20,05 SAY "Numero da Remessa    : " GET ARQUIVO
         @ 21,05 SAY "C¢digo da Carteira   : " GET COD_CARTE  PICT "@R 99999"
	 READ
	 UNLOCK
      ENDIF
      PLQUA("01")
      PPERG ( "Continua (S/N) ? ")
      IF sisRESP = "S"
	 LOOP
      ELSE
	 EXIT
      ENDIF
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA04
RETURN nil
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_CONF_COB
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
******************************************************************
