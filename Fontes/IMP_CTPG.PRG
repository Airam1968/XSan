*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : IMP_CTPG.PRG
* Data        : 01/01/1999
* Objetivo    : Captura dos Dados de Contas a Pagar
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION IMP_CTPG()
SAVE SCREEN TO TELA05
PLQUA("11")
PTIT( "IMPORTAÄ«O DADOS CONTAS A PAGAR EM TEXTO")
eDRIVE     = SPACE(5)
eSUBDIR    = SPACE(20)
eDIR       = SPACE(40)
SELE 1
USE STATUS 
GOTO TOP
IF F_BLOQR()
	@ 07,00 SAY "Drive de Dados    : " GET D_IMPCP PICTURE "@!XXX"
	@ 08,00 SAY "SubDiret¢rio      : " GET P_IMPCP
	@ 09,00 SAY "Extená∆o          : " GET E_IMPCP PICTURE "@!XXX"
	READ
	UNLOCK
ENDIF
eDRIVE     = D_IMPCP
eSUBDIR    = P_IMPCP
eDIR       = ALLTRIM(eDRIVE)+":"+ALLTRIM(eSUBDIR)
eCORI_RET  = E_IMPCP
PLQUA("01")
PPERG ( "Deseja Capturar o Arquivo (S/N) ? ")
   IF sisRESP = "S"
      PLQUA("01")
      PMENSAG("Aguarde a Captura dos Arquivos Dispon°veis ...")
      DO WHILE .T.
	 * Box de escolha do arquivo
         eDIR = ALLTRIM(ALLTRIM(eDRIVE)+":"+ALLTRIM(eSUBDIR)+"*."+ALLTRIM(eCORI_RET))
         @ 11,00                                                  
	 @ 11,00 SAY "SubDiret¢rio      : " GET eDIR
	 ARQ_TXT = ADIR(eDIR)
	 eOPCAO  = ARQ_TXT
	 DECLARE vARQUIVO[ARQ_TXT]
	 ADIR(eDIR,vARQUIVO)
	 ASORT(vARQUIVO)
	 SAVE SCREEN TO TELA06
	 @ 12,25 CLEAR TO 21,40
	 @ 12,25 TO 21,40 DOUBLE
	 DO WHILE .T.
	    @ 13,05 SAY "Arquivos     : " 
	    MENU_ARQ = ACHOICE(13,27,20,39,vARQUIVO)
	    DO CASE
	       CASE LASTKEY() == 13
		  eNOME_ARQ = vARQUIVO[MENU_ARQ]
		  EXIT
	    ENDCASE
	 ENDDO
	 RESTORE SCREEN FROM TELA06
	 @ 14,00 SAY "Arquivo Selecionado  : " GET eNOME_ARQ 
	 CLEAR GETS
	 EXIT
      ENDDO  
	
   * Transferencia para arquivo DBF
   PLQUA("01")
   PMENSAG("Aguarde a Preparaá∆o do Arquivo ...")
   SELE 2
   SET EXCLUSIVE ON
   USE TRANSCPG
   ZAP
   GOTO TOP
   IF F_BLOQA()
      PLQUA("01")
      PPERG ( "Deseja Carregar o Arquivo (S/N) ? ")
      IF sisRESP = "S"
	 SELE 1
	 eNOME_ARQ = ALLTRIM(eNOME_ARQ)
	 SELE 2
	 DELETE ALL
	 PACK
	 GOTO TOP
	 vARQUIVO = ALLTRIM(eDRIVE)+":"+ALLTRIM(eSUBDIR)+ALLTRIM(eNOME_ARQ)
	 APPEND FROM &vARQUIVO SDF
	 GOTO TOP
      ENDIF
      GOTO TOP
      DELETE
      PACK
      REPLACE ALL DESCRICAO WITH SUBSTR(DESCRICAO,2,40)
      UNLOCK
      SELE 2
      USE
      USE TRANSCPG        
      GOTO TOP      
      SET EXCLUSIVE OFF      
      PLQUA("01")
      PMSGR( "Importaá∆o Conclu°da !",3)      
   ENDIF
ENDIF
PLQUA("01")
PPERG ( "Deseja Visualizar os Registros Capturados (S/N) ? ")
IF sisRESP = "S"
   PLQUA("01")
        SELE 2
        USE
        USE TRANSCPG        
	GOTO TOP
	PLIMPA()
	SET COLOR TO W/N,+W/R
	DECLARE CAMPO[FCOUNT()],PIC[FCOUNT()],CAB[FCOUNT()],LENS[FCOUNT()]
	AFIELDS(CAMPO,.T.,LENS)
	PIC[1] ="99"
	PIC[2] ="999999"
	PIC[3] ="999"
	PIC[4] ="999999"
	PIC[5] ="99999999"
	PIC[6] ="999,999,999.99"
	PIC[7] ="@S40"
	PIC[8] ="99"
	PIC[9] ="999999999"
	PIC[10]="99999999"
	PIC[11]="9999999999"
	PIC[12]="99999999"
	PIC[13]="999,999,999.99"
	PIC[14]="999999999"	
	PIC[15]="9999999999"	

	CAB[1] ="C‡DIGO"
	CAB[2] ="DESPESA"
	CAB[3] ="EMPREENDIMENTO"
	CAB[4] ="FORNECEDOR"
	CAB[5] ="DATA"
	CAB[6] ="VALOR"
	CAB[7] ="DESCRIÄ«O"
	CAB[8] ="CONTA"	
	CAB[9] ="NOTA FISCAL"
	CAB[10]="DATA NF"
	CAB[11]="NUM COMPRA"
	CAB[12]="DATA PAGTO"
	CAB[13]="VLR.PAGTO"
	CAB[14]="NUM.CHEQUE"		
	CAB[15]="NUM.LANÄAMENTO"

	@ 5,00 TO 21,79 DOUBLE
	PLQUA("01")
	SET COLOR TO
	SET COLOR TO W/N+*
	@ 23,26 SAY "[ESC]"
	@ 23,59 SAY "["+CHR(26)+CHR(27)+CHR(24)+CHR(25)+"]"
	SET COLOR TO
	SET COLOR TO W/N,+W/R
	@ 23,31 SAY "Para Sair"
	@ 23,65 SAY "Movimentaá∆o"
	DBEDIT(6,01,21,78,CAMPO,"F_INDIIMP",PIC,CAB,CHR(196),CHR(179),CHR(205))
 
ENDIF  

PLQUA("01") 
PPERG("Confirma o processamento dos registros (S/N) ? ")
IF sisRESP = "S"
    PLQUA("01")  
    SELE 1
    USE CPAGAR INDEX CPAGAR,DATA_CPG,DATA_CPV,NUM_CHEQ,DATA_NF
    GOTO TOP
    SELE 5
    USE CLAS_PRO INDEX CLAS_PRO,DESC_PRO
    GOTO TOP
    SELE 4
    USE CLASSE_D INDEX CLASSE_D
    GOTO TOP
    SELE 3
    USE STATUS
    GOTO TOP
    SELE 6
    USE FORNECED INDEX FORNECED
    GOTO TOP	    
    SELE 2
    GOTO TOP
    DO WHILE .NOT. EOF()
       eCLASSE_D = 0
       eEMPREENDIM = 0
       eFORNECED = 0
       eDATA = ""
       eVALOR = 0
       eDESCRICAO = ""
       eCONTA = 0
       eNOTAFISCAL = 0
       eDATANF = ""
       eNUMCOMPRA = ""
       eDATAPAGTO = ""
       eVLRPAGTO = 0
       eNUMCHEQUE = ""	       
       eNUM_LANC = 0
       *
       SELE 2
       eCLASSE_D = DESPESA
       eEMPREENDIM = EMPREENDIM
       eFORNECED = FORNECED
       eDATA = ALLTRIM(DATA)
       eDATA = SUBSTR(eDATA,1,2)+"/"+SUBSTR(eDATA,3,2)+"/"+SUBSTR(eDATA,5,4)     
       eVALOR = VALOR
       eDESCRICAO = DESCRICAO
       eCONTA = CONTA
       eNOTAFISCAL = NF
       eDATANF = ALLTRIM(DATANF)
       eDATANF = SUBSTR(eDATANF,1,2)+"/"+SUBSTR(eDATANF,3,2)+"/"+SUBSTR(eDATANF,5,4)       
       eNUMCOMPRA = NUMCOMPRA
       eDATAPAGTO = ALLTRIM(DATA_PGTO)
       eDATAPAGTO = SUBSTR(eDATAPAGTO,1,2)+"/"+SUBSTR(eDATAPAGTO,3,2)+"/"+SUBSTR(eDATAPAGTO,5,4)
       eVLRPAGTO = VLR_PGTO
       eNUMCHEQUE = NUM_CHEQUE        
       *
       lOK = .T.
       SELE 4
       GOTO TOP
       SEEK eCLASSE_D
       IF .NOT. FOUND()
	  lOK = .F.
	  PLQUA("01")
	  PMENSAG( "Despesa N∆o Encontrada ("+ALLTRIM(STR(eCLASSE_D))+")!")      
          PPARA()
       ENDIF
       *
       SELE 6
       GOTO TOP
       SEEK eFORNECED
       IF .NOT. FOUND()
	  lOK = .F.
	  PLQUA("01")
	  PMENSAG( "Fornecedor N∆o Encontrado ("+ALLTRIM(STR(eFORNECED))+")!")
          PPARA()	  
       ENDIF
       *
       SELE 5
       GOTO TOP
       SEEK eEMPREENDIM
       IF .NOT. FOUND()
	  lOK = .F.
	  PLQUA("01")
	  PMENSAG( "Empreendimento N∆o Encontrado ("+ALLTRIM(STR(eEMPREENDIM))+")!")
          PPARA()	  
       ENDIF
       *
       
       IF .NOT. lOK
          SELE 2
          SKIP
          LOOP
       ENDIF   
       
       *
       SELE 3
       GOTO TOP
       IF F_BLOQR()
	  eNUM_LANC  = LANC_CPG
	  REPLACE LANC_CPG WITH eNUM_LANC + 1
	  UNLOCK
       ENDIF               
       *
       *
       PLQUA("01")
       PMENSAG("Gravando ("+ALLTRIM(STR(eNUM_LANC))+"), Aguarde ...")
       SELE 1
       APPEND BLANK
       IF F_BLOQR()
	  REPLACE LANCAMENTO WITH eNUM_LANC,;
		  CLASSE_D   WITH eCLASSE_D
	  REPLACE EMPREENDIM WITH eEMPREENDIM,;
		  CODIGO_FOR WITH eFORNECED
	  REPLACE DESCRICAO  WITH eDESCRICAO,;
		  DATA_PREV  WITH CTOD(eDATA),;
		  PREVISTO   WITH eVALOR
	  REPLACE DATA_REAL  WITH CTOD(eDATAPAGTO),;
		  REALIZADO  WITH eVLRPAGTO,;
		  COD_CONTA  WITH eCONTA
	  REPLACE NUM_CHEQUE WITH eNUMCHEQUE,;
		  NUM_DOCUME WITH eNOTAFISCAL,;
		  CONTABIL   WITH .T.
	  REPLACE DATA_NOTA  WITH CTOD(eDATANF),;
		  NOMINAL    WITH "S",;
		  INTEGRACAO WITH ""
	  REPLACE NUM_COMPRA WITH eNUMCOMPRA
	  UNLOCK
	  
       ENDIF               
       *
       SELE 2
       IF F_BLOQR()
	  REPLACE LANCAMENTO WITH eNUM_LANC
	  UNLOCK
       ENDIF               
       SELE 2
       SKIP
    ENDDO
      PLQUA("01")
      PMSGR( "Processamento Conclu°do !",3)         
 ENDIF  
CLOSE ALL
RESTORE SCREEN FROM TELA05
RETURN nil
*************************  fim  **************************************
***************************** funcao *********************************
FUNCTION F_INDIIMP                
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 27
         RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
