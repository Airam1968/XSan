*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CAPT_SIG.PRG
* Data        : 01/01/1999
* Objetivo    : Captura dos Dados de Pagamento do SIGMA-ITAU
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION CAPT_SIG()
SAVE SCREEN TO TELA05
PTIT( "CAPTURA DOS DADOS SIGMA-÷TAU")
SELE 1
USE CONF_SIG
GOTO TOP
SELE 2
USE TRANSTXT
GOTO TOP
SELE 1
PLQUA("01")
PPERG ( "Confirma (S/N) ? ")
IF sisRESP = "S"
   DO WHILE .T.
      SELE 1
      PLQUA("11")
      IF F_BLOQR()
	 @ 06,05 SAY "Data Configuraá∆o : " GET DATA
	 @ 08,05 SAY "Drive de Dados    : " GET DRIVE PICT "@! X"
	 @ 10,05 SAY "SubDiretário      : " GET SUBDIR
	 CLEAR GETS
	 eDRIVE = DRIVE
	 eSUBDIR = SUBDIR
	 UNLOCK
      ENDIF
      PLQUA("01")
      PMENSAG("Aguarde a Captura dos Arquivos Dispon°veis ...")
      DO WHILE .T.
	 * Box de escolha do arquivo
	 eDIR = ALLTRIM(eDRIVE)+":"+ALLTRIM(eSUBDIR)+"*.0*"
	 ARQ_TXT = ADIR(eDIR)
	 eOPCAO  = ARQ_TXT
	 DECLARE ARQUIVO[ARQ_TXT]
	 ADIR(eDIR,ARQUIVO)
	 ASORT(ARQUIVO)
	 SAVE SCREEN TO TELA06
	 @ 11,25 CLEAR TO 20,40
	 @ 11,25 TO 20,40 DOUBLE
	 DO WHILE .T.
	    @ 12,05 SAY "Arquivo do SIGMA  : " 
	    MENU_ARQ = ACHOICE(12,27,20,39,ARQUIVO,eOPCAO)
	    DO CASE
	       CASE LASTKEY() == 13
	       eNOME_ARQ = ARQUIVO[MENU_ARQ]
	       EXIT
	    ENDCASE
	 ENDDO
	 RESTORE SCREEN FROM TELA06
	 @ 12,05 SAY "Arquivo do SIGMA  : " GET eNOME_ARQ 
	 CLEAR GETS
	 PLQUA("01")
	 PPERG ( "Confirma (S/N) ? ")
	 IF sisRESP = "S"
	    EXIT
	 ELSE
	    LOOP
	 ENDIF
      ENDDO  
      * Transferencia para arquivo DBF
      PLQUA("01")
      PMENSAG("Aguarde a Preparaá∆o do Arquivo ...")
      SELE 2
      SET EXCLUSIVE ON
      USE TRANSTXT
      GOTO TOP
      IF F_BLOQA()
	 DELETE ALL
	 PACK
	 GOTO TOP
	 ARQUIVO = ALLTRIM(eDRIVE)+":"+ALLTRIM(eSUBDIR)+ALLTRIM(eNOME_ARQ)
	 APPEND FROM &ARQUIVO SDF
	 GOTO TOP
	 DELE ALL FOR CGCE_TRN01 <> 51665396000127
	 PACK
	 REPLACE ALL VLTI_TRN01 WITH VLTI_TRN01/100,;
		     VIOF_TRN01 WITH VIOF_TRN01/100,;
		     VABA_TRN01 WITH VABA_TRN01/100
	 REPLACE ALL VDES_TRN01 WITH VDES_TRN01/100,;
		     VPRI_TRN01 WITH VPRI_TRN01/100,;
		     JMMU_TRN01 WITH JMMU_TRN01/100
	 REPLACE ALL OUCR_TRN01 WITH OUCR_TRN01/100,;
		     VOSA_TRN01 WITH VOSA_TRN01/100
	 UNLOCK
      ENDIF
      SET EXCLUSIVE OFF
      PLQUA("01")
      PPERG ( "Confirma (S/N) ? ")
      IF sisRESP = "S"
	 EXIT
      ELSE
	 LOOP
      ENDIF
   ENDDO
ENDIF
CLOSE ALL
RESTORE SCREEN FROM TELA05
RETURN nil
*************************  fim  *********************************
