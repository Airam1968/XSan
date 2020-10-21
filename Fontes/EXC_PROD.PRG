*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_PROD.PRG
* Data        : 01/01/99
* Objetivo    : EXCLUSAO DE PRODUTOS PARA VENDA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
function EXC_PROD()
SAVE SCREEN TO TELA03
PTIT( "EXCLUSAO DE PRODUTOS")
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
USE CAD_VEND INDEX CAD_VEND
GOTO TOP
SELE 2
* Selecao do produto
DO WHILE .T.
   PLIMPA()
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 05,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(5,15,5,70,CAMPO,"KEYS_4",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
DO CASE
   CASE eTIPO_PRO = 1
      * Alteracao de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA   = 0
      @ 06,02 SAY  sisGRUPO1+' : '
      GO TOP
      DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]='@R99999'
      PIC[2]='@R99999'
      PIC[3]='@R99999'
      PIC[4]='@S99999'
      CAB[1]='    '
      CAB[2]='    '
      CAB[3]='    '
      CAB[4]='    '
      DBEDIT(6,14,6,45,CAMPO,'KEYS_4',PIC,CAB,' ',' - ',' ')
      eQUADRA     = NUM_QUADRA
      eNUM_INICIO = NUM_INICIO
      eNUM_FINAL  = NUM_FINAL
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
      GOTO TOP
      DO PLQUA WITH '01'
      DO PPERG WITH 'Confirma a Exclusao '+sisUNID1+' (S/N) ? '
      IF sisRESP = 'S'
         SELE 5 
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
         GOTO TOP
         IF EOF()
            SELE 1 
	    DO PLQUA WITH '01'
            DO PMENSAG WITH 'Aguarde a Eliminacao dos Registros ...'
            IF F_BLOQA()
	       DELETE ALL
	       UNLOCK
	    ENDIF
	    SELE 3
	    SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5)
	    IF F_BLOQR()
               REPLACE CADASTRO WITH .F.
	       UNLOCK
	    ENDIF
	    F_LACESSO(sisUSUARIO,'CADASTRO CLIENTES','EXCLUSAO DO PRODUTO CLASSE : '+ALLTRIM(STR(eCLAS_PRO,5))+' QUA/BLO : '+ALLTRIM(eQUADRA))
	 ELSE
           DO PLQUA WITH '01'
           DO PMENSAG WITH 'Exclus∆o Bloqueada, Existem vendas neste Produto '	 
           DO PPARA
	 ENDIF
      ENDIF
   CASE eTIPO_PRO = 2
      * Alteracao de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = 0
      @ 06,02 SAY  sisGRUPO2+' : '
      GO TOP
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]='@R99999'
      PIC[2]='@R99999'
      PIC[3]='@R99999'
      CAB[1]='    '
      CAB[2]='    '
      CAB[3]='    '
      DBEDIT(6,08,6,76,CAMPO,'KEYS_4',PIC,CAB,' ',' - ',' ')               
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
      GOTO TOP
      DO PLQUA WITH '01'
      DO PPERG WITH 'Confirma a Exclusao '+sisUNID2+' (S/N) ? '
      IF sisRESP = 'S'
         SELE 5 
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
         GOTO TOP
         IF EOF() 
            SELE 1          
            DO PLQUA WITH '01'
	    DO PMENSAG WITH 'Aguarde a Eliminacao dos Registros ...'
	    IF F_BLOQA()
	       DELETE ALL
	       UNLOCK
	    ENDIF
	    SELE 4
	    SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5)
	    IF F_BLOQR()
	       REPLACE CADASTRO WITH .F.
	       UNLOCK
	    ENDIF
	    F_LACESSO(sisUSUARIO,'CADASTRO CLIENTES','EXCLUSAO DO PRODUTO CLASSE : '+ALLTRIM(STR(eCLAS_PRO,5))+' QUA/BLO : '+ALLTRIM(eBLOCO))
	 ELSE
           DO PLQUA WITH '01'
           DO PMENSAG WITH 'Exclus∆o Bloqueada, Existem vendas neste Produto '	 
           DO PPARA
	 ENDIF	    
      ENDIF
   CASE eTIPO_PRO = 3
      * Alteracao de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eBLOCO    = 0
      @ 06,02 SAY 'Bloco  : '
      GO TOP
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]='@R99999'
      PIC[2]='@R99999'
      PIC[3]='@R99999'
      CAB[1]='    '
      CAB[2]='    '
      CAB[3]='    '
      DBEDIT(6,10,6,41,CAMPO,'KEYS_4',PIC,CAB,' ',' - ',' ')
      eBLOCO      = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      SELE 1
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
      GOTO TOP
      DO PLQUA WITH '01'
      DO PPERG WITH 'Confirma a Exclusao das Unidades (S/N) ? '
      IF sisRESP = 'S'
         SELE 5 
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO
         GOTO TOP
         IF EOF()      
            SELE 1       
            DO PLQUA WITH '01'
	    DO PMENSAG WITH 'Aguarde a Eliminacao dos Registros ...'
	    IF F_BLOQA()
	       DELETE ALL
	       PACK
	       UNLOCK
	    ENDIF
	    SELE 4
	    SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5)
	    IF F_BLOQR()
	       REPLACE CADASTRO WITH .F.
	       UNLOCK
	    ENDIF
	    F_LACESSO(sisUSUARIO,'CADASTRO CLIENTES','EXCLUSAO DO PRODUTO CLASSE : '+ALLTRIM(STR(eCLAS_PRO,5))+' QUA/BLO : '+ALLTRIM(eBLOCO))
	 ELSE
           DO PLQUA WITH '01'
           DO PMENSAG WITH 'Exclus∆o Bloqueada, Existem vendas neste Produto '	
           DO PPARA
	 ENDIF	 
      ENDIF
   OTHERWISE
      DO PDESENV
ENDCASE

CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************

