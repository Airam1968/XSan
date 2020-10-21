*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REL_RECE.PRG
* Data        : 01/01/99
* Objetivo    : RELATORIO DE RECEBIMENTOS NO PERIODO   
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REL_RECE()
SAVE SCREEN TO TELA03
PTIT( "RELATàRIO DE RECEBIMENTOS")
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
GOTO TOP
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
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SELE 6
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 7
USE CONTAS INDEX CONTAS
GOTO TOP
SELE 8
* Cadastro de Parcelas
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
   DBEDIT(5,15,5,70,CAMPO,"FUNC_CONS",PIC,CAB," "," - "," ")
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   eDESC_PRO = DESCRICAO
   EXIT
ENDDO
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 07,02 SAY "Periodo : " GET eDATA_INI
   @ 07,25 SAY "Ate : "     GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv lidas !",3)
      LOOP
   ELSE
      IF eDATA_INI > eDATA_FIM
         PLQUA("01")
         PMSGR( "Data Inicio > Que Final !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
ENDDO
PLQUA("01")
eORIGEM = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Origem dos Pagamentos (C)arteira,(B)anco ou (A)mbas ? " GET eORIGEM PICT "@!X" VALID eORIGEM $("CBA")
READ
eTIPO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Tipo de Parcelas (S)inal,(N)ormais,(I)ntermediarias ou (A)mbas ? " GET eTIPO PICT "@!X" VALID eTIPO $("SNIA")
READ
ePROTESTO = SPACE(1)
PLQUA("01")
@ 23,00 SAY "Selecionar T‡tulos ‡ (P)rotesto,(N)‡o Protesto ou (A)mbos ? " GET ePROTESTO PICT "@!X" VALID ePROTESTO $("PNA")
READ
PLQUA("01")
eORDEM = SPACE(1)
@ 23,00 SAY "Emiss‡o de Relat‡rio por (O)rdem de Pagamento ou (P)roduto(O/P) ? " GET eORDEM PICT "@!X" VALID eORDEM $("OI")
READ
PLQUA("01")
ePERIODO = SPACE(1)
@ 23,00 SAY "Selecionar Pagamentos (A)ntecipado, (P)ostergado, (M)es ou (T)odos (A/P/M/T) ? " GET ePERIODO PICT "@!X" VALID ePERIODO $("APMT")
READ
DO WHILE .T.
   eCONTA = 0
   PLQUA("01")
   @ 23,00 SAY "Informe o No. da Conta Corrente ou (0) para Todas ? " GET eCONTA PICT "@R 999"
   READ
   SELE 7
   SEEK eCONTA
   IF FOUND()
      EXIT
   ELSE
      IF eCONTA = 0
         EXIT
      ELSE
         PLQUA("01")
         PMSGR( "Aten‡Æo !",3)
         PMENSAG("Conta NÆo Existe !")
         PPARA()
         LOOP
      ENDIF
   ENDIF
ENDDO
eARQUIVO = SPACE(60)
IF ALLTRIM(sisDRIVE_REL) <> SPACE(1)
	eARQUIVO = sisDRIVE_REL+":"
ELSE
	eARQUIVO = "C:"
ENDIF	

IF sisPASTA_REL <> SPACE(30)
        eARQUIVO = ALLTRIM(eARQUIVO) + sisPASTA_REL+"EXTRATO.TXT"+SPACE(10)  
ELSE
        eARQUIVO = ALLTRIM(eARQUIVO) + "EXTRATO.TXT"+SPACE(10) 
ENDIF    
eCLIENTE = .F.
PLQUA("01")
PPERG ( "Deseja imprimir o Nome\CPF\CNPJ do Cliente (S/N) ? ")
IF sisRESP = "S"
   eCLIENTE = .T.
ENDIF
IF eTIPO_PRO = 1
   SELE 3
   SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5)
   GOTO TOP
ELSE
   SELE 4
   SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5)
   GOTO TOP
ENDIF
eSUB_TOT  = 0
eTOT_TOT  = 0
eTOTAL    = 0
SELE 9
SET EXCLUSIVE ON
USE TEMP_PAR
IF F_BLOQA()
   ZAP
   GOTO TOP
   UNLOCK
ENDIF
SET EXCLUSIVE OFF
IF eTIPO_PRO = 1
   SELE 3
ELSE
   SELE 4
ENDIF
DO WHILE .NOT. EOF()
   IF eTIPO_PRO = 1
      SELE 3
      eQUADRA = NUM_QUADRA
   ELSE
      SELE 4
      eQUADRA = NUM_BLOCO
   ENDIF
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   SELE 9
   USE TEMP_PAR
   IF F_BLOQA()
      DO CASE
         CASE ePERIODO = "T"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                           .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                           .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                           .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                              .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                              .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                              .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                              .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                              .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM);
                              .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         CASE ePERIODO = "A"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC < DATA_INI) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         CASE ePERIODO = "P"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. DATA_VENC > eDATA_FIM .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
         CASE ePERIODO = "M"
            DO CASE
               CASE eTIPO = "A"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "N"
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "N" .OR. SUBSTR(TIPO_PARC,1,1) = "R") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "I" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "I") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
               CASE eTIPO = "S" 
                  IF eORIGEM = "A"
                     DO CASE
                        CASE ePROTESTO = "A"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL ;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "P"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. PROTESTO;
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        CASE ePROTESTO = "N"
                           APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. .NOT. PROTESTO ; 
                           .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                     ENDCASE
                  ELSE 
                     IF eORIGEM = "C"
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B";
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = "B" .AND. .NOT. PROTESTO;           
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ELSE
                        DO CASE
                           CASE ePROTESTO = "A"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " "; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "P"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. PROTESTO; 
                             .AND. (SUBSTR(TIPO_PARC,1,1) = "S")  .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                           CASE ePROTESTO = "N"
                              APPEND FROM &eARQ FOR (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM .AND. VLR_PGTO <> 0) .AND. CONTABIL .AND. ORIGEM = " " .AND. .NOT. PROTESTO; 
                              .AND. (SUBSTR(TIPO_PARC,1,1) = "S") .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. IIF(eCONTA <> 0,CONTA = eCONTA,CONTA <> 1000)
                        ENDCASE
                     ENDIF
                  ENDIF
            ENDCASE
      ENDCASE
      GOTO TOP
      UNLOCK
   ENDIF
   IF eTIPO_PRO = 1
      SELE 3
      SKIP
   ELSE
      SELE 4
      SKIP
   ENDIF
ENDDO
PMENSAG("Posicione o Formulario 80 Colunas e Tecle [ENTER]")
PPARA()
DO PIMPR
SET DEVICE TO PRINT
SELE 9
USE TEMP_PAR
INDEX ON DATA_VENC TO TEMP_PAR
GOTO TOP
eDIA = 10
DO WHILE .NOT. EOF()
   @ 01,00  SAY sisLETRAC+"Empresa : "+sisTIT1
   @ 01,115 SAY "Data : "+dtoc(DATE())
   @ 02,00  SAY "Sistema : "+sisTIT2
   @ 02,115 SAY "Relat: SAN004"
   @ 03,20  SAY sisLETRAE+"RELATORIO RECEBIMENTOS DE "+DTOC(eDATA_INI)+" ATE "+DTOC(eDATA_FIM)+sisLETRAC
   @ 04,00  SAY "Produto : "+STR(eCLAS_PRO,5)+" - "+ALLTRIM(eDESC_PRO)
   @ 05,00  SAY "Periodo : "+IIF(ePERIODO = "T","Todos",IIF(ePERIODO = "A","Antecipado",IIF(ePERIODO = "P","Postergado","do Mes")))
   @ 05,30  SAY "Origem : "+IIF(eORIGEM = "A","Ambas",IIF(eORIGEM = "C","Carteira","Bancaria"))
   @ 05,60  SAY "Tipo : "+IIF(eTIPO = "A","Todas",IIF(eTIPO = "S","Sinal",IIF(eTIPO = "I","Intermediarias","Normais")))
   @ 05,90  SAY "Protesto : "+IFF(ePROTESTO = "A","Ambas",IIF(ePROTESTO = "P","Protesto","Nao Protesto"))
   @ 06,00  SAY REPLICATE("=",135)
   @ 07,02  SAY "No. IMOVEL"
   @ 07,15  SAY "DATA VENC."
   @ 07,27  SAY "TIPO"
   @ 07,35  SAY "PARC."
   @ 07,41  SAY "CONTA"
   @ 07,50  SAY "VLR. PARCELA"
   @ 08,00 SAY REPLICATE("=",135)
   LIN = 9
   DO WHILE .NOT. EOF() .AND. LIN <= 54
      IF DAY(DATA_VENC) <= eDIA
         IF eCLIENTE
            eCLASSE     = CLASSE
            eQUADRA     = QUADRA
            eNUM_IMOVEL = NUM_IMOVEL
            ePARTE      = PARTE
            SELE 6
            SEEK CLASSE+QUADRA+NUM_IMOVEL+STR(PARTE,1)
            eNUM_CLIE = NUM_CLIE
            eDATA_VENDA = DATA_VENDA
            SELE 5
            SEEK eNUM_CLIE
            eNOME = CLIENTE
            eCPF_CGC = CPF_CGC            
            SELE 9
            @ LIN,02  SAY ALLTRIM(eNOME) + " - CPF\CNPJ : " + ALLTRIM(eCPF_CGC) + " - DATA VENDA : " + +DTOC(eDATA_VENDA)
            LIN = LIN + 1
         ENDIF
         @ LIN,02  SAY ALLTRIM(CLASSE)+"/"+STR(PARTE,1)+"/"+ALLTRIM(QUADRA)+"/"+ALLTRIM(NUM_IMOVEL)
         @ LIN,15  SAY DATA_VENC
         @ LIN,29  SAY TIPO_PARC
         @ LIN,36  SAY ALLTRIM(NUM_PARC)
         @ LIN,44  SAY VLR_PARC    PICT "@R 999,999,999,999.99"
         eSUB_TOT  = eSUB_TOT  + VLR_PARC
         LIN = LIN + 1
         SKIP
      ELSE
         @ LIN,42  SAY REPLICATE("_",43)
         LIN = LIN + 1
         @ LIN,02  SAY "Subtotal : "
         @ LIN,64  SAY eSUB_TOT PICT "@R 999,999,999,999.99"
         eTOTAL = eTOTAL  + eSUB_TOT
         eSUB_TOT = 0
         LIN = LIN + 2
         IF eDIA = 10
            eDIA = 20
         ELSE
            eDIA = 31
         ENDIF
      ENDIF
      DO PTECLA
      IF sisINTERR
         SET DEVICE TO SCREEN
         CLOSE ALL
         RETURN nil
      ENDIF
   ENDDO
   IF EOF()
      @ LIN,42  SAY REPLICATE("_",43)
      LIN = LIN + 1
      @ LIN,02  SAY "Subtotal : "
      @ LIN,64  SAY eSUB_TOT PICT "@R 999,999,999,999.99"
      eTOTAL = eTOTAL  + eSUB_TOT
      eSUB_TOT = 0
      LIN = LIN + 2
      @ LIN,42  SAY REPLICATE("_",43)
      LIN = LIN + 1
      @ LIN,02  SAY "Total Geral : "
      @ LIN,64  SAY eTOTAL PICT "@R 999,999,999,999.99"
      LIN = LIN + 2
   ENDIF
   @ 56,00 SAY REPLICATE("=",135)
   @ 57,95 SAY sisTIT3
   @ 01,00 SAY " "+sisLETRAN
ENDDO
SET PRINT TO
SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************

